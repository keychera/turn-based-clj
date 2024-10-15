(ns engine2.timeline
  (:require [clojure.walk :refer [prewalk]]
            [com.rpl.specter :as sp]))

(require '[pod.huahaiy.datalevin :as d])

;; datalevin
(def Rule
  #:rule
   {:rule-name  {:db/unique    :db.unique/identity
                 :db/valueType :db.type/keyword}
    :activation {:db/cardinality :db.cardinality/one}
    :rule-fn    {:db/cardinality :db.cardinality/one}})

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
   {:actor-name {:db/cardinality :db.cardinality/one
                 :db/valueType   :db.type/keyword}
    :hp         {:db/cardinality :db.cardinality/one}
    :mp         {:db/cardinality :db.cardinality/one}
    :effects    {:db/valueType   :db.type/ref
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

(def timeline-schema (merge Rule Moment Action Actor Effect))

;; all 'timeline' argument here is a datalevin conn
;; q- prefix = query functions
;; ! suffix  = function with db d/transact!

(defn q-moment [timeline epoch]
  (d/q '[:find (pull ?moment [*]) .
         :in $ ?epoch
         :where [?moment :moment.attr/epoch ?epoch]]
       (d/db timeline) epoch))

(defn q-last-epoch [timeline]
  (d/q '[:find (max ?last-epoch) .
         :where [?a :moment.attr/epoch ?last-epoch]]
       (d/db timeline)))

(defn q-last-moment [timeline]
  (q-moment timeline (q-last-epoch timeline)))

(defn make-new-entity [m]
  (prewalk (fn [node]
             (if (map? node)
               (if (= '(:db/id) (keys node))
                 (str "id-" (->> node vals first))
                 (update node :db/id #(str "id-" %)))
               node)) m))

;; specter paths
(defn entity [actor-name]
  [:moment.attr/entities sp/ALL #(= actor-name (:actor.attr/name %))])

(defn effect-on [actor-name effect-name]
  [:moment.attr/entities sp/ALL #(= actor-name (:actor.attr/name %))
   :actor.attr/effects sp/ALL #(= effect-name (:effect.attr/effect-name %))])

;; timeline core
(defn engrave-action!
  [timeline moves {:action.attr/keys [actor act-expr] :as action}]
  (let [[move move-attr] act-expr
        move-attr        (-> move-attr
                             (assoc :move.attr/actor actor)
                             (assoc :move.attr/move-name move))
        alter            (get moves move)
        moment           (make-new-entity (q-last-moment timeline))
        new-moment       (->> (alter moment move-attr)
                              (sp/transform [:moment.attr/epoch] inc)
                              (sp/setval    [:moment.attr/events]
                                            [(update action :action.attr/act-expr str)]))]
    (d/transact! timeline [new-moment])))

(defn engrave! [timeline world history]
  (let [{:world/keys [moves effects initial]} world
        moves (-> moves (update-vals eval))]
    (d/transact! timeline [initial])
    (loop [[action & remaining-actions] history]
      (engrave-action! timeline moves action)
      (if (empty? remaining-actions)
        ["last-moment" (q-last-moment timeline)]
        (recur remaining-actions)))))
