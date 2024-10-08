(ns engine2.try-datalevin
  (:require [babashka.fs :as fs]
            [clojure.walk :refer [prewalk]]
            [com.rpl.specter :as sp]
            [babashka.pods :as pods]))

(defonce datalevin-pod
  (do (pods/load-pod 'huahaiy/datalevin "0.9.10")
      :datalevin-pod))

(require '[pod.huahaiy.datalevin :as d])
;; let's just use datalevin the way it's supposed to be used

(def Moment
  #:moment.attr
   {:epoch {:db/cardinality :db.cardinality/one}
    :entities {:db/valueType   :db.type/ref
               :db/cardinality :db.cardinality/many
               :db/isComponent true}})

(def Actor
  #:actor.attr
   {:name    {:db/cardinality :db.cardinality/one
              :db/valueType :db.type/keyword}
    :hp      {:db/cardinality :db.cardinality/one}
    :mp      {:db/cardinality :db.cardinality/one}
    :effects {:db/valueType   :db.type/ref
              :db/cardinality :db.cardinality/many
              :db/isComponent true}})

(def Effect
  #:effect.attr
   {:effect-name {:db/cardinality :db.cardinality/one
                  :db/valueType :db.type/keyword}
    :duration    {:db/cardinality :db.cardinality/one}
    :source      {:db/valueType :db.type/ref}})

(def schema (merge Moment Actor Effect))

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


(comment
  (def battle-conn (d/get-conn "tmp/datalevin/rpg" schema))
  (d/transact! battle-conn
               [#:moment.attr
                 {:epoch     0
                  :entities [#:actor.attr{:name :actor/aluxes
                                          :hp   1000
                                          :mp   45}
                             #:actor.attr{:name :actor/hilda
                                          :hp   700
                                          :mp   400}]}])

  (d/transact! battle-conn [[:db/retractEntity [:moment.attr/epoch 0]]])

  ;; query all
  (d/q '[:find ?a ?b ?c
         :where [?a ?b ?c]]
       (d/db battle-conn))

  ;; query hilda on epoch 0
  (d/q '[:find (pull ?eid [*]) .
         :in $ ?epoch ?actor
         :where
         [?moment :moment.attr/epoch ?epoch]
         [?moment :moment.attr/entities ?eid]
         [?eid :actor.attr/name ?actor]]
       (d/db battle-conn)
       0 :actor/hilda)

  ;; query epoch 
  ;; db now can have multiple moment with same epoch (idea for branch history)
  (->> (get-moment battle-conn 0)
       (sp/transform [:moment.attr/epoch] inc)
       (sp/setval    [:moment.attr/desc] "hilda attacked!")
       (sp/transform [(entity :actor/hilda)]
                     (comp #(update % :actor.attr/hp - 100)
                           #(update % :actor.attr/mp - 100)))
       (conj [])
       (d/transact! battle-conn))

  ;; create new moment
  (let [prev-epoch      0
        current-moment (d/q '[:find (pull ?moment [*]) .
                              :in $ ?epoch
                              :where [?moment :moment.attr/epoch ?epoch]]
                            (d/db battle-conn)
                            prev-epoch)
        alter          (fn [moment]
                         (->> moment
                              dissoc-all-dbid
                              (sp/transform [:moment.attr/epoch] inc)
                              (sp/transform [:moment.attr/entities sp/ALL #(= :actor/hilda (:actor.attr/name %)) :actor.attr/hp] #(- % 100))))
        new-moment     (alter current-moment)]
    (d/transact! battle-conn [new-moment]))


  (d/close battle-conn)
  (fs/delete-tree "tmp/datalevin/rpg"))
