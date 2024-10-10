(ns engine2.try-datalevin
  (:require [babashka.fs :as fs]
            [clojure.walk :refer [prewalk]]
            [com.rpl.specter :as sp]))

(require '[pod.huahaiy.datalevin :as d])

(def Moment
  #:moment.attr
   {:epoch    {:db/cardinality :db.cardinality/one}
    :desc     {:db/valueType :db.type/string}
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
   {:actor    {:db/valueType :db.type/keyword}
    :act-expr {:db/valueType :db.type/string}})

(def Effect
  #:effect.attr
   {:effect-name {:db/cardinality :db.cardinality/one
                  :db/valueType   :db.type/keyword}
    :duration    {:db/cardinality :db.cardinality/one}
    :source      {:db/valueType :db.type/ref}})



(def timeline-schema (merge Moment Action Actor Effect))


(defn make-new-entity [m]
  (prewalk (fn [node]
             (if (map? node)
               (if (= '(:db/id) (keys node))
                 (str "id-" (->> node vals first))
                 (update node :db/id #(str "id-" %)))
               node)) m))

(defn get-moment [conn epoch]
  (->> (d/q '[:find (pull ?moment [*]) .
              :in $ ?epoch
              :where [?moment :moment.attr/epoch ?epoch]]
            (d/db conn) epoch)))

(defn entity [actor-name]
  [:moment.attr/entities sp/ALL #(= actor-name (:actor.attr/name %))])

(defn effect-on [actor-name effect-name]
  [:moment.attr/entities sp/ALL #(= actor-name (:actor.attr/name %))
   :actor.attr/effects sp/ALL #(= effect-name (:effect.attr/effect-name %))])



;; hilda

(defn basic-attack [actor target]
  (fn [moment]
    (let [damage 50]
      (->> moment
           (sp/setval    [:moment.attr/desc] (str actor " attacks " target " for " damage " damage!"))
           (sp/transform [(entity target)]
                         #(update % :actor.attr/hp - damage))))))

(defn fireball [actor target]
  (fn [moment]
    (let [mp-cost 15 damage 50]
      (->> moment
           (sp/setval    [:moment.attr/desc] (str actor " cast fireball towards " target " for " damage " damage!"))
           (sp/transform [(entity target)]
                         (comp #(update % :actor.attr/mp - mp-cost)
                               #(update % :actor.attr/hp - damage)))))))

(defn poison
  ([actor target] (poison actor target #:effect.attr{:duration 3}))
  ([actor target {:effect.attr/keys [duration]}]
   (fn [moment]
     (let [mp-cost 30 effect-name :debuff/poison
           actor-entity (sp/select-one [(entity :char/hilda)] moment)
           return (->> moment
                       (sp/setval    [:moment.attr/desc] (str actor " poisons " target " ! " target " is now poisoned!"))
                       (sp/transform [(entity actor)]
                                     #(update % :actor.attr/mp - mp-cost))
                       (sp/setval    [(effect-on target :debuff/poison)]
                                     #:effect.attr{:effect-name effect-name
                                                   :source (:db/id actor-entity)
                                                   :duration duration}))]
       return))))

(def initial-moment
  #:moment.attr
   {:epoch    0
    :entities [#:actor.attr
                {:db/id "hilda"
                 :name :char/hilda
                 :hp   700
                 :mp   400}
               #:actor.attr
                {:name    :char/aluxes
                 :hp      1000
                 :mp      45
                 :effects [#:effect.attr
                            {:effect-name :debuff/poison
                             :source      "hilda"
                             :duration    1}]}]})

(def history
  [#:action.attr
    {:actor  :char/aluxes
     :act-expr '(poison :char/hilda :char/aluxes)}
   #:action.attr
    {:actor  :char/aluxes
     :act-expr '(basic-attack :char/aluxes :char/hilda)}
   #:action.attr
    {:actor  :char/aluxes
     :act-expr '(fireball :char/hilda :char/aluxes)}])


(comment
  (def last-rand (atom 0))

  (#_:init-rand-db
   let [rand-id (rand 42) _ (reset! last-rand rand-id)
        database (str "tmp/rpg-" rand-id)
        timeline (d/get-conn database timeline-schema)]
   (try (println "init timeline on" database)
        (finally (d/close timeline))))

  (#_:query-all
   let [timeline (d/get-conn (str "tmp/rpg-" @last-rand) timeline-schema)]
   (try (d/q '[:find ?a ?b ?c :where [?a ?b ?c]] (d/db timeline))
        (finally (d/close timeline))))

  (#_:get-specific-moment
   let [timeline (d/get-conn (str "tmp/rpg-" @last-rand) timeline-schema)]
   (try (->> (get-moment timeline 3))
        (finally (d/close timeline))))

  basic-attack fireball poison

  (let [timeline (d/get-conn (str "tmp/rpg-" @last-rand) timeline-schema)]
    (try
      (d/transact! timeline [initial-moment])
      (loop [epoch 0
             [action & remaining-actions] history]
        (let [{:action.attr/keys [act-expr]} action
              alter (eval act-expr)
              moment (make-new-entity (get-moment timeline epoch))
              new-moment (->> (alter moment)
                              (sp/transform [:moment.attr/epoch] inc)
                              (sp/setval    [:moment.attr/events] [(update action :action.attr/act-expr str)]))]
          (d/transact! timeline [new-moment])
          (if (empty? remaining-actions)
            :done
            (recur (inc epoch) remaining-actions))))
      (finally (d/close timeline))))

  (fs/delete-tree "tmp"))
