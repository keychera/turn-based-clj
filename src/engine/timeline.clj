(ns engine.timeline
  (:require [com.rpl.specter :as sp :refer [select setval]]))

;; Engine

(defn entities->effect-data [entities]
  (->> entities
       (mapcat (fn [[affected attr]]
                 (->> (:attr/effect attr)
                      (map (fn [[effect-name effect-data]]
                             (assoc effect-data
                                    :effect-data/effect-name effect-name
                                    :effect-data/affected affected))))))))

(defn reduce-effect-duration [{:effect-data/keys [duration affected effect-name]} state]
  (let [new-duration (dec duration)]
    (cond
      (= new-duration 0) (->> state (setval [:state/entities affected :attr/effect effect-name] sp/NONE))
      :else              (->> state (setval [:state/entities affected :attr/effect effect-name :effect-data/duration] new-duration)))))

(defmulti unleash-effect :effect-data/effect-name)

(defmethod unleash-effect :default [_] nil)

(defn reduce-effects [original-timeline moment event]
  (let [state (peek original-timeline)
        all-affected (select [:state/entities sp/ALL (sp/selected? (fn [[_ attr]] (:attr/effect attr)))] state)
        effects (entities->effect-data all-affected)]
    (->> effects
         (reduce (fn [timeline effect-data]
                   (let [state (peek timeline)
                         whose (:moment/whose moment)
                         new-moment (unleash-effect (assoc effect-data
                                                           :effect-data/whose whose
                                                           :effect-data/event event
                                                           :effect-data/state state))]
                     (cond-> timeline
                       (some? new-moment) (conj new-moment))))
                 original-timeline))))

(defn do-eval [namespace-sym form]
  (let [user-ns (create-ns namespace-sym)]
    (binding [*ns* user-ns] (clojure.core/eval form))))

(defn reduce-timeline
  ([model initial-state history]
   (reduce-timeline initial-state history model (count history)))
  ([model initial-state history limit]
   (->> history
        (take (min limit (count history)))
        (reduce (fn [timeline {:moment/keys [whose action] :as moment}]
                  (let [alter (do-eval model action)
                        state (peek timeline)
                        timeline0 (conj timeline (-> state
                                                     (update :state/moment inc)
                                                     (assoc :state/desc (str "new moment for" whose))))
                        timeline1 (reduce-effects timeline0 moment :event/on-moment-begins)
                        state (peek timeline1)
                        timeline2 (conj timeline1 (alter state))
                        timeline3 (reduce-effects timeline2 moment :event/on-moment-ends)]
                    timeline3))
                [initial-state]))))

(comment

  (add-tap #(def last-tap %))
  (add-tap #(println %))
  last-tap)