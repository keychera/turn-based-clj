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

(defn reduce-effects [original-timeline event]
  (let [state (peek original-timeline)
        all-affected (select [:state/entities sp/ALL (sp/selected? (fn [[_ attr]] (:attr/effect attr)))] state)
        effects (entities->effect-data all-affected)]
    (->> effects
         (reduce (fn [timeline effect-data]
                   (let [state (peek timeline)
                         new-moment (unleash-effect (assoc effect-data :effect-data/event event :effect-data/state state))]
                     (cond-> timeline
                       (some? new-moment) (conj new-moment))))
                 original-timeline))))

(defn do-eval [ns-symbol form]
  (require ns-symbol)
  (let [user-ns (create-ns ns-symbol)]
    (binding [*ns* user-ns] (clojure.core/eval form))))

(defn reduce-timeline
  ([model initial-state battle-data]
   (reduce-timeline model initial-state battle-data Integer/MAX_VALUE))
  ([model initial-state battle-data turn-limit]
   (let [{:battle-data/keys [num-moment-per-turn history-atom]} battle-data
         history @history-atom]
     (loop [timeline [initial-state] limit turn-limit remaining-turns history]
       (if (or (= limit 0) (empty? remaining-turns))
         timeline
         (let [moments-per-turn (take num-moment-per-turn remaining-turns)
               remaining-turns (drop num-moment-per-turn remaining-turns)
               state (peek timeline)
               new-timeline (conj [] (-> state (update :state/turn inc)))
               new-timeline (reduce-effects new-timeline :event/on-turn-begins)
               new-timeline
               (loop [moment-timeline new-timeline
                      [moment & remaining-moments] moments-per-turn]
                 (let [{:moment/keys [action]} moment
                       moment-timeline (reduce-effects moment-timeline :event/on-moment-begins)
                       alter (do-eval model action)
                       state (peek moment-timeline)
                       moment-timeline (conj moment-timeline (alter state))
                       moment-timeline (reduce-effects moment-timeline :event/on-moment-ends)]
                   (if (empty? remaining-moments)
                     moment-timeline
                     (recur moment-timeline remaining-moments))))
               new-timeline (reduce-effects new-timeline :event/on-turn-ends)
               new-timeline (drop 1 new-timeline)
               updated-timeline (into timeline new-timeline)]
           (recur updated-timeline (dec limit) remaining-turns)))))))

(comment
  (add-tap #(def last-tap %))
  (add-tap #(println %))
  last-tap)