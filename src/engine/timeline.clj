(ns engine.timeline
  (:require [engine.triplestore :refer [overwrite-entity remove-triples transform-entity]]
            [pod.huahaiy.datalevin :as d]))


;; Engine
(defn reduce-effect-duration [moment {:effect-data/keys [affected effect-id duration]}]
  (let [new-duration (dec duration)]
    (cond (= duration -1) moment
          (= new-duration 0) (-> moment
                                 (remove-triples [affected :attr/effects effect-id])
                                 (remove-triples [effect-id '_ '_]))
          :else (transform-entity moment effect-id {:effect-data/duration new-duration}))))

(def poison
  [[:info/moment :moment/whose :actor/aluxes]
   [:actor/aluxes :attr/effects 1]
   [:actor/hilda :attr/effects 1]
   [:actor/aluxes :attr/effects 2]
   [1 :effect-data/effect-name :debuff/poison]
   [1 :effect-data/source :actor/hilda]
   [1 :effect-data/duration 3]])

(d/q '[:find [?affected ?source ?eid ?duration]
       :where
       [:info/moment :moment/whose ?affected]
       [?affected :attr/effects ?eid]
       [?eid :effect-data/effect-name :debuff/poison]
       [?eid :effect-data/source ?source]
       [?eid :effect-data/duration ?duration]]
     poison)


(defn reduce-effects [original-timeline effects action]
  (let [emerging-moment (overwrite-entity (peek original-timeline) :info/moment action)
        effect-timeline
        (loop [current-timeline [] [effect & remaining-effects] effects]
          (let [current-moment (or (peek current-timeline) emerging-moment)
                {:effect/keys [activation-query unleash]} effect
                effect-result (when (some? effect) (d/q activation-query current-moment))
                new-moment (when (some? effect-result) (unleash current-moment effect-result))
                current-timeline (cond-> current-timeline
                                   (some? new-moment) (conj new-moment))]
            (if (empty? remaining-effects) current-timeline
                (recur current-timeline remaining-effects))))]
    (cond-> original-timeline
      (not-empty effect-timeline) (into effect-timeline))))

(defn do-eval [ns-symbol form]
  (require ns-symbol)
  (let [user-ns (create-ns ns-symbol)]
    (binding [*ns* user-ns] (clojure.core/eval form))))

(defn reduce-timeline
  ([model initial-moment battle-data]
   (reduce-timeline model initial-moment battle-data Integer/MAX_VALUE))
  ([model initial-moment battle-data turn-limit]
   (let [{:battle-data/keys [num-actions-per-turn active-effects history-atom]} battle-data
         history @history-atom]
     (loop [timeline [initial-moment] limit turn-limit remaining-actions history]
       (if (or (= limit 0) (empty? remaining-actions))
         timeline
         (let [actions-per-turn (take num-actions-per-turn remaining-actions)
               remaining-actions (drop num-actions-per-turn remaining-actions)
               moment (peek timeline)
               new-timeline (conj [] (cond-> (transform-entity moment :info/timeline {:timeline/turn inc})
                                       (empty? remaining-actions) (conj [:info/timeline :timeline/last-turn? true])))
               new-timeline (loop [moment-timeline new-timeline
                                   [action-moment & remaining-action] actions-per-turn]
                              (let [{:moment/keys [whose action]} action-moment
                                    moment-timeline (reduce-effects moment-timeline active-effects #:moment{:whose whose :event :event/on-moment-begins})
                                    alter (do-eval model action)
                                    moment (peek moment-timeline)
                                    new-moment (-> moment (overwrite-entity :info/moment action-moment) (alter))
                                    moment-timeline (conj moment-timeline new-moment)
                                    moment-timeline (reduce-effects moment-timeline active-effects  #:moment{:whose whose :event :event/on-moment-ends})]
                                (if (empty? remaining-action)
                                  moment-timeline
                                  (recur moment-timeline remaining-action))))
               new-timeline (drop 1 new-timeline)
               updated-timeline (into timeline new-timeline)]
           (recur updated-timeline (dec limit) remaining-actions)))))))

(comment
  (require '[model.hilda :refer [initial-moment battle-data]])
  (add-tap #(def last-tap %))
  (add-tap println)
  last-tap

  (reduce-timeline 'model.hilda initial-moment battle-data 2))