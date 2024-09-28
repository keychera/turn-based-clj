(ns engine.timeline
  (:require [engine.triplestore :refer [get-entity remove-triples
                                        transform-entity]]
            [pod.huahaiy.datalevin :as d]))


;; Engine
(defn reduce-effect-duration [moment {:effect-data/keys [affected effect-id duration]}]
  (let [new-duration (dec duration)]
    (cond (= duration -1) moment
          (= new-duration 0) (-> moment
                                 (remove-triples [affected :attr/effects effect-id])
                                 (remove-triples [effect-id '_ '_]))
          :else (transform-entity moment effect-id {:effect-data/duration new-duration}))))

(defmulti unleash-effect :effect-data/effect-name)

(defmethod unleash-effect :default [_] nil)

(defn reduce-effects [original-timeline event]
  (let [moment (peek original-timeline)
        effects-id (d/q '[:find ?affected ?effect-id :where [?affected :attr/effects ?effect-id]] moment)]
    (->> effects-id
         (map (fn [[affected effects-id]] [affected effects-id (get-entity moment effects-id)]))
         (reduce (fn [timeline [affected effect-id effect-data]]
                   (let [moment (peek timeline)
                         extra-data #:effect-data{:affected affected :effect-id effect-id :event event :moment moment}
                         new-moment (unleash-effect (merge effect-data extra-data))]
                     (cond-> timeline
                       (some? new-moment) (conj new-moment))))
                 original-timeline))))

(defn do-eval [ns-symbol form]
  (require ns-symbol)
  (let [user-ns (create-ns ns-symbol)]
    (binding [*ns* user-ns] (clojure.core/eval form))))

(defn reduce-timeline
  ([model initial-moment battle-data]
   (reduce-timeline model initial-moment battle-data Integer/MAX_VALUE))
  ([model initial-moment battle-data turn-limit]
   (let [{:battle-data/keys [num-moment-per-turn history-atom]} battle-data
         history @history-atom]
     (loop [timeline [initial-moment] limit turn-limit remaining-turns history]
       (if (or (= limit 0) (empty? remaining-turns))
         timeline
         (let [moments-per-turn (take num-moment-per-turn remaining-turns)
               remaining-turns (drop num-moment-per-turn remaining-turns)
               moment (peek timeline)
               new-timeline (conj [] (cond-> (transform-entity moment :info/timeline {:timeline/turn inc})
                                       (empty? remaining-turns) (conj [:info/timeline :timeline/last-turn? true])))
               new-timeline (reduce-effects new-timeline :event/on-turn-begins)
               new-timeline (loop [moment-timeline new-timeline
                                   [moment & remaining-moments] moments-per-turn]
                              (let [{:moment/keys [action]} moment
                                    moment-timeline (reduce-effects moment-timeline :event/on-moment-begins)
                                    alter (do-eval model action)
                                    moment (peek moment-timeline)
                                    new-moment (alter moment)
                                    moment-timeline (conj moment-timeline new-moment)
                                    moment-timeline (reduce-effects moment-timeline :event/on-moment-ends)]
                                (if (empty? remaining-moments)
                                  moment-timeline
                                  (recur moment-timeline remaining-moments))))
               new-timeline (reduce-effects new-timeline :event/on-turn-ends)
               new-timeline (drop 1 new-timeline)
               updated-timeline (into timeline new-timeline)]
           (recur updated-timeline (dec limit) remaining-turns)))))))

(comment
  (require '[model.hilda :refer [initial-moment battle-data]])
  (add-tap #(def last-tap %))
  (add-tap #(println %))
  last-tap

  (reduce-timeline 'model.hilda initial-moment battle-data 2))