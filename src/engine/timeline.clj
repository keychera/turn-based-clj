(ns engine.timeline
  (:require [clojure.walk :refer [prewalk]]
            [com.rpl.specter :as sp]
            [pod.huahaiy.datalevin :as d]))

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
               :db/valueType   :db.type/keyword}
    :facts    {:db/cardinality :db.cardinality/many
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
   {:action-name {:db/valueType :db.type/keyword}
    :actor-name  {:db/valueType :db.type/keyword}})

(def Effect
  #:effect.attr
   {:effect-name {:db/cardinality :db.cardinality/one
                  :db/valueType   :db.type/keyword}
    :duration    {:db/cardinality :db.cardinality/one}
    :source-name  {:db/valueType :db.type/keyword}})

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
  (sp/comp-paths :moment.attr/entities sp/ALL #(= actor-name (:actor.attr/actor-name %))))

(defn entity-id [actor-id]
  (sp/comp-paths :moment.attr/entities sp/ALL #(= actor-id (:db/id %))))

(def add-effect
  (sp/comp-paths
   :actor.attr/effects sp/NIL->VECTOR sp/AFTER-ELEM))

(defn on-effect [effect-name source-name]
  (sp/comp-paths
   :actor.attr/effects sp/ALL
   #(and (= effect-name (:effect.attr/effect-name %))
         (= source-name (:effect.attr/source-name %)))))

(defn on-or-add-effect [effect-name source-name]
  (sp/comp-paths
   (sp/if-path
    (on-effect effect-name source-name)
    (on-effect effect-name source-name) add-effect)))

;; timeline core
(defn unleash-rules! [timeline unleashed-rules]
  (loop [moment                       (q-last-moment timeline)
         [unleashed-rule & remaining] unleashed-rules]
    (let [[activated rule] unleashed-rule
          rule-fn          (:rule/rule-fn rule)
          silent?          (:rule/silent? rule)
          new-moment       (if silent?
                             (rule-fn moment activated)
                             (-> moment
                                 (assoc :moment.attr/facts [])
                                 (rule-fn activated)))]
      (prn ["unleashing" (:rule/rule-name rule) "with" activated])
      (if (some? remaining)
        (recur new-moment remaining)
        (if silent?
          (do (d/transact! timeline [[:db.fn/retractEntity (:db/id new-moment)]])
              (d/transact! timeline [new-moment]))
          (d/transact! timeline [(make-new-entity (sp/transform [:moment.attr/epoch] inc new-moment))]))))))

(defn engrave-rules!
  [timeline rules actor-name timing]
  (loop [current-epoch (q-last-epoch timeline)
         rules rules]
    (let [activation-compl [['?s.current-moment :moment.attr/epoch current-epoch]
                            :in '$ '% '?s.who-acts '?s.timing
                            #_#_:timeout 2000]
          grouped          (->> rules
                                (mapv (fn [{:rule/keys [rules-to-inject activation] :as rule}]
                                        (let [query-stmt (apply conj activation activation-compl)
                                              activated  (try (d/q query-stmt (d/db timeline) rules-to-inject actor-name timing)
                                                              (catch Throwable e
                                                                (prn ["error on query:" (dissoc (Throwable->map e) :trace) rule])))]
                                          [activated rule])))
                                (group-by (fn [[activated _]]
                                            (if (and (some? activated) (seq activated))
                                              :engrave!/unleashed-rules
                                              :engrave!/dormant-rules))))
          unleashed-rules  (:engrave!/unleashed-rules grouped)
          dormant-rules    (:engrave!/dormant-rules grouped)]
      (when (some? unleashed-rules)
        (unleash-rules! timeline unleashed-rules))
      (if (and (some? unleashed-rules) (some? dormant-rules))
        (recur (inc current-epoch) (map second dormant-rules))
        :engrave!/done))))

(defn engrave-action!
  [timeline moves {:action.attr/keys [action-name] :as action-attr}]
  (let [alter            (get moves action-name)
        moment           (->> (q-last-moment timeline)
                              (sp/setval    [:moment.attr/facts] [action-attr]))
        new-moment       (->> (alter moment action-attr)
                              (sp/transform [:moment.attr/epoch] inc))]
    (d/transact! timeline [(make-new-entity new-moment)])))

(defn engrave! [timeline world history]
  (let [{:world/keys [actions rules initial]} world
        actions (-> actions (update-vals eval))]
    (d/transact! timeline [initial])
    (loop [[action & remaining-actions] history]
      (let [actor-name (:action.attr/actor-name action)]
        (engrave-rules! timeline rules actor-name :timing/before-action)
        (engrave-action! timeline actions action)
        (engrave-rules! timeline rules actor-name :timing/after-action)
        (if (empty? remaining-actions)
          ["last-moment" (q-last-moment timeline)]
          (recur remaining-actions))))))
