(ns engine2.timeline
  (:require [clojure.walk :refer [prewalk]]
            [com.rpl.specter :as sp]))

(require '[pod.huahaiy.datalevin :as d])

;; datalevin
(def Rule
  #:rule
   {:name       {:db/unique    :db.unique/identity
                 :db/valueType :db.type/keyword}
    :activation {:db/cardinality :db.cardinality/one}
    :unleash-fn {:db/cardinality :db.cardinality/one}})

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

(def timeline-schema (merge Rule Moment Action Actor Effect))

(defn get-moment [conn epoch]
  (->> (d/q '[:find (pull ?moment [*]) .
              :in $ ?epoch
              :where [?moment :moment.attr/epoch ?epoch]]
            (d/db conn) epoch)))

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
(defn do-eval [ns-symbol form]
  (require ns-symbol)
  (let [user-ns (create-ns ns-symbol)]
    (binding [*ns* user-ns] (clojure.core/eval form))))

(defn engrave! [history initial-moment under-namespace]
  (let [timeline (d/get-conn (str "tmp/rpg") timeline-schema)]
    (try
      (d/transact! timeline [initial-moment])
      (loop [epoch 0
             [action & remaining-actions] history]
        (let [{:action.attr/keys [act-expr]} action
              alter (do-eval under-namespace act-expr)
              moment (make-new-entity (get-moment timeline epoch))
              new-moment (->> (alter moment)
                              (sp/transform [:moment.attr/epoch] inc)
                              (sp/setval    [:moment.attr/events] [(update action :action.attr/act-expr str)]))]
          (d/transact! timeline [new-moment])
          (if (empty? remaining-actions)
            ["last-moment" (get-moment timeline (inc epoch))]
            (recur (inc epoch) remaining-actions))))
      (finally (d/close timeline)))))