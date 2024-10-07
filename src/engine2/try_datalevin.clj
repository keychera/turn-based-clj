(ns engine2.try-datalevin
  (:require [babashka.fs :as fs]))

(require '[babashka.pods :as pods])
(pods/load-pod 'huahaiy/datalevin "0.9.10")
(require '[pod.huahaiy.datalevin :as d])

;; let's just use datalevin the way it's supposed to be used

(def Moment
  #:moment.attr
   {:turn {:db/unique :db.unique/identity}
    :entities {:db/valueType   :db.type/ref
               :db/cardinality :db.cardinality/many
               :db/isComponent true}})

(def Actor
  #:actor.attr
   {:name    {:db/valueType :db.type/keyword}
    :hp      {}
    :mp      {}
    :effects {:db/valueType   :db.type/ref
              :db/cardinality :db.cardinality/many
              :db/isComponent true}})

(def Effect
  #:effect.attr
   {:effect-name {:db/valueType :db.type/keyword}
    :duration    {}
    :source      {:db/valueType :db.type/ref}})

(def schema (merge Moment Actor Effect))

(comment
  (def conn (d/get-conn "tmp/datalevin/rpg" schema))
  (d/transact! conn
               [#:moment.attr
                 {:turn     0
                  :entities [#:actor.attr{:name :actor/aluxes
                                          :hp   1000
                                          :mp   45}
                             #:actor.attr{:name :actor/hilda
                                          :hp   700
                                          :mp   400}]}])

  (d/transact! conn [[:db/retractEntity [:moment.attr/turn 0]]])

  ;; query all
  (d/q '[:find ?a ?b ?c
         :where [?a ?b ?c]]
       (d/db conn))

  ;; query hilda on turn 0
  (d/q '[:find (pull ?eid [*])
         :in $ ?turn ?actor
         :where
         [?moment :moment.attr/turn ?turn]
         [?moment :moment.attr/entities ?eid]
         [?eid :actor.attr/name ?actor]]
       (d/db conn)
       0 :actor/hilda)


  (d/close conn)
  (fs/delete-tree "tmp/datalevin/rpg"))
