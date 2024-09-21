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
  (require namespace-sym)
  (let [user-ns (create-ns namespace-sym)]
    (binding [*ns* user-ns] (clojure.core/eval form))))

(defn reduce-timeline
  ([model initial-state history]
   (reduce-timeline model initial-state history (count history)))
  ([model initial-state history limit]
   (->> history
        (take (min limit (count history)))
        (reduce (fn [timeline {:moment/keys [action] :as moment}]
                  (let [alter (do-eval model action)
                        state (peek timeline)
                        new-timeline (conj [] (-> state (update :state/moment inc)))
                        new-timeline (reduce-effects new-timeline moment :event/on-moment-begins)
                        state (peek new-timeline)
                        new-timeline (conj new-timeline (alter state))
                        new-timeline (reduce-effects new-timeline moment :event/on-moment-ends)
                        new-timeline (drop 1 new-timeline)]
                    (into timeline new-timeline)))
                [initial-state]))))

(comment
  (add-tap #(def last-tap %))
  (add-tap #(println %))
  last-tap)