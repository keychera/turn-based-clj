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
    :timing   {:db/cardinality :db.cardinality/one
               :db/valueType :db.type/keyword}
    :events   {:db/cardinality :db.cardinality/many
               :db/valueType   :db.type/ref
               :db/isComponent true}
    :entities {:db/cardinality :db.cardinality/many
               :db/valueType   :db.type/ref
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

(defn- make-new-entity [m]
  (prewalk (fn [node]
             (if (map? node)
               (let [map-some    (fn [[k v]] (when v [k v]))
                     nil-removed (into {} (map map-some node))
                     new-entity  (->> nil-removed
                                      (sp/transform [:db/id some?] #(str "id-" %))
                                      (sp/setval [:db/id nil?] sp/NONE))]
                 new-entity)
               node)) m))

;; specter paths
(defn entity [actor-name]
  [:moment.attr/entities sp/ALL #(= actor-name (:actor.attr/name %))])

(defn entity-id [actor-id]
  [:moment.attr/entities sp/ALL #(= actor-id (:db/id %))])

(defn on-effect [effect-name]
  [:actor.attr/effects sp/ALL #(= effect-name (:effect.attr/effect-name %))])


;; timeline core


(defn unleash-rules! [timeline unleashed-rules]
  (loop [moment                       (-> (q-last-moment timeline)
                                          (assoc :moment.attr/events []))
         [unleashed-rule & remaining] unleashed-rules]
    (let [[activated rule] unleashed-rule
          rule-fn          (:rule/rule-fn rule)
          new-moment       (rule-fn moment activated)]
      (prn ["unleashing!!!" rule new-moment])
      (tap> new-moment)
      (if (some? remaining)
        (recur new-moment remaining)
        (do (prn ["transacting!" (make-new-entity new-moment)])
            (d/transact! timeline [(make-new-entity (sp/transform [:moment.attr/epoch] inc new-moment))])
            (prn ["done transacting!"]))))))

(defn engrave-rules!
  [timeline rules timing]
  (loop [current-epoch (q-last-epoch timeline)
         rules (filter #(= (:rule/timing %) timing) rules)]
    (let [{:engrave!/keys [unleashed-rules dormant-rules]}
          (->> rules
               (mapv (fn [{:rule/keys [activation] :as rule}]
                       (let [query-stmt (conj activation ['?current-moment :moment.attr/epoch current-epoch])
                             activated  (d/q query-stmt (d/db timeline))]
                         [activated rule])))
               (group-by (fn [[activated _]]
                           (if (some? activated)
                             :engrave!/unleashed-rules
                             :engrave!/dormant-rules))))]
      (when (some? unleashed-rules)
        (prn ["unleashed!" unleashed-rules dormant-rules])
        (unleash-rules! timeline unleashed-rules))
      (if (and (some? unleashed-rules) (some? dormant-rules))
        (recur (inc current-epoch) dormant-rules)
        :engrave!/done))))

(defn engrave-action!
  [timeline moves {:action.attr/keys [actor act-expr] :as action}]
  (let [[move move-attr] act-expr
        move-attr        (-> move-attr
                             (assoc :move.attr/actor actor)
                             (assoc :move.attr/move-name move))
        alter            (get moves move)
        moment           (q-last-moment timeline)
        _ (prn ["moment of " (:moment.attr/epoch moment)])
        new-moment       (->> (alter moment move-attr)
                              (sp/transform [:moment.attr/epoch] inc)
                              (sp/setval    [:moment.attr/events]
                                            [(update action :action.attr/act-expr str)]))]

    (d/transact! timeline [(make-new-entity new-moment)])))

(defn engrave! [timeline world history]
  (let [{:world/keys [moves rules initial]} world
        moves (-> moves (update-vals eval))]
    (d/transact! timeline [initial])
    (loop [[action & remaining-actions] history]
      (prn "before-action!")
      (engrave-rules! timeline rules :timing/before-action)
      (prn "action!")
      (engrave-action! timeline moves action)
      (prn "after-action!")
      (engrave-rules! timeline rules :timing/after-action)
      (if (empty? remaining-actions)
        ["last-moment" (q-last-moment timeline)]
        (recur remaining-actions)))))
