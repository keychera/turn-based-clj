(ns engine2.try-datalevin
  (:require [babashka.fs :as fs]
            [clojure.walk :refer [prewalk]]
            [com.rpl.specter :as sp]))

(require '[pod.huahaiy.datalevin :as d])

(def Moment
  #:moment.attr
   {:epoch    {:db/cardinality :db.cardinality/one}
    :events   {:db/valueType   :db.type/ref
               :db/cardinality :db.cardinality/many
               :db/isComponent true}
    :entities {:db/valueType   :db.type/ref
               :db/cardinality :db.cardinality/many
               :db/isComponent true}})

(def Actor
  #:actor.attr
   {:name    {:db/cardinality :db.cardinality/one
              :db/valueType   :db.type/keyword}
    :hp      {:db/cardinality :db.cardinality/one}
    :mp      {:db/cardinality :db.cardinality/one}
    :effects {:db/valueType   :db.type/ref
              :db/cardinality :db.cardinality/many
              :db/isComponent true}})

(def Action
  #:action.attr
   {:actor     {:db/valueType :db.type/keyword}
    :statement {:db/valueType :db.type/symbol}})

(def Effect
  #:effect.attr
   {:effect-name {:db/cardinality :db.cardinality/one
                  :db/valueType   :db.type/keyword}
    :duration    {:db/cardinality :db.cardinality/one}
    :source      {:db/valueType :db.type/ref}})



(def timeline-schema (merge Moment Action Actor Effect))

(defn dissoc-all-dbid [m]
  (prewalk (fn [node] (cond-> node (map? node) (dissoc :db/id))) m))

(defn get-moment [conn epoch]
  (->> (d/q '[:find (pull ?moment [*]) .
              :in $ ?epoch
              :where [?moment :moment.attr/epoch ?epoch]]
            (d/db conn) epoch)
       dissoc-all-dbid))

(defn entity [actor-name]
  [:moment.attr/entities sp/ALL #(= actor-name (:actor.attr/name %))])

(defn effect-on [actor-name effect-name]
  [:moment.attr/entities sp/ALL #(= actor-name (:actor.attr/name %))
   :action.attr/effects sp/ALL #(= effect-name (:actor.attr/name %))])


;; hilda

(defn basic-attack [actor target]
  (fn [moment]
    (let [damage 50]
      (-> moment
          (sp/setval    [:moment.attr/desc] (str actor " attacks " target " for " damage " damage!"))
          (sp/transform [(entity target)]
                        #(update % :actor.attr/hp - damage))))))

(defn fireball [actor target]
  (fn [moment]
    (let [mp-cost 15 damage 50]
      (-> moment
          (sp/setval    [:moment.attr/desc] (str actor " cast fireball towards " target " for " damage " damage!"))
          (sp/transform [(entity target)]
                        (comp #(update % :actor.attr/mp - mp-cost)
                              #(update % :actor.attr/hp - damage)))))))

(defn poison
  ([actor target] (poison actor target #:effect.attr{:duration 3}))
  ([actor target {:effect.attr/keys [duration]}]
   (fn [moment]
     (let [mp-cost 30 effect-name :debuff/poison]
       (-> moment
           (sp/setval    [:moment.attr/desc] (str actor " poisons " target " ! " target " is now poisoned!"))
           (sp/transform [(entity actor)]
                         #(update % :actor.attr/mp - mp-cost))
           (sp/setval    [(effect-on target :debuff/poison)]
                         #:effect.attr{:effect-name effect-name
                                       :source actor
                                       :duration duration}))))))

(def initial-moment
  #:moment.attr
   {:epoch    0
    :entities [#:actor.attr
                {:name    :char/aluxes
                 :hp      1000
                 :mp      45
                 :effects [#:effect.attr
                            {:effect-name :debuff/poison
                             :source      "hilda"
                             :duration    1}]}
               #:actor.attr
                {:db/id "hilda"
                 :name :char/hilda
                 :hp   700
                 :mp   400}]})

(def history
  [#:action.attr
    {:actor  :char/aluxes
     :action '(poison :char/hilda :char/aluxes)}
   #:action.attr
    {:actor  :char/aluxes
     :action '(basic-attack :char/aluxes :char/hilda)}
   #:action.attr
    {:actor  :char/aluxes
     :action '(fireball :char/hilda :char/aluxes)}])


(comment
  (def last-rand (atom 0))

  (#_:init-rand-db
   let [rand-id (rand 42) _ (reset! last-rand rand-id)
        database (str "tmp/rpg-" rand-id)
        timeline (d/get-conn database timeline-schema)]
   (try (println "init timeline on" database)
        (finally (d/close timeline))))

  (#_:single-transact!
   let [timeline (d/get-conn (str "tmp/rpg-" @last-rand) timeline-schema)]
   (try (d/transact! timeline [#:moment.attr{:epoch 0}])
        (finally (d/close timeline))))

  (#_:query-all
   let [timeline (d/get-conn (str "tmp/rpg-" @last-rand) timeline-schema)]
   (try (d/q '[:find ?a ?b ?c :where [?a ?b ?c]] (d/db timeline))
        (finally (d/close timeline))))

  basic-attack fireball poison

  (let [timeline (d/get-conn (str "tmp/rpg-" @last-rand) timeline-schema)]
    (try
      (d/transact! timeline [initial-moment])
      (loop [epoch 0
             [action & remaining-actions] history]
        (let [moment (get-moment timeline epoch)
              new-moment (->> moment
                              (sp/transform [:moment.attr/epoch] inc)
                              (sp/setval    :moment.attr/entities sp/NONE)
                              (sp/setval    [:moment.attr/events] [action]))]
          (println ["transcating..." action])
          (d/transact! timeline [new-moment])
          (if (empty? remaining-actions)
            :done
            (recur (inc epoch) remaining-actions))))
      (finally (d/close timeline))))

  (def timeline-conn (d/get-conn "tmp/datalevin/rpg" timeline-schema))
  (d/transact! timeline-conn
               [#:moment.attr
                 {:epoch     0
                  :entities [#:actor.attr{:name :char/aluxes
                                          :hp   1000
                                          :mp   45}
                             #:actor.attr{:name :char/hilda
                                          :hp   700
                                          :mp   400}]}])

  (d/transact! timeline-conn [[:db/retractEntity 1]])

  (d/transact! timeline-conn [[:db/retract 1 :moment.attr/entities]])

  ;; query all
  (d/q '[:find ?a ?b ?c
         :where [?a ?b ?c]]
       (d/db timeline-conn))

  ;; query hilda on epoch 0
  (d/q '[:find (pull ?eid [*]) .
         :in $ ?epoch ?actor
         :where
         [?moment :moment.attr/epoch ?epoch]
         [?moment :moment.attr/entities ?eid]
         [?eid :actor.attr/name ?actor]]
       (d/db timeline-conn)
       0 :char/hilda)

  ;; query epoch 
  ;; db now can have multiple moment with same epoch (idea for branch history)
  (->> (get-moment timeline-conn 0)
       (sp/transform [:moment.attr/epoch] inc)
       (sp/setval    [:moment.attr/desc] "hilda attacked!")
       (sp/transform [(entity :char/hilda)]
                     (comp #(update % :actor.attr/hp - 100)
                           #(update % :actor.attr/mp - 100)))
       (conj [])
       (d/transact! timeline-conn))

  ;; create new moment
  (let [prev-epoch      0
        current-moment (d/q '[:find (pull ?moment [*]) .
                              :in $ ?epoch
                              :where [?moment :moment.attr/epoch ?epoch]]
                            (d/db timeline-conn)
                            prev-epoch)
        alter          (fn [moment]
                         (->> moment
                              dissoc-all-dbid
                              (sp/transform [:moment.attr/epoch] inc)
                              (sp/transform [:moment.attr/entities sp/ALL #(= :char/hilda (:actor.attr/name %)) :actor.attr/hp] #(- % 100))))
        new-moment     (alter current-moment)]
    (d/transact! timeline-conn [new-moment]))


  (d/close timeline-conn)
  (fs/delete-tree "tmp/datalevin/rpg"))
