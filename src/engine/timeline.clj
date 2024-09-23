(ns engine.timeline
  (:require [engine.triplestore :refer [gen-dynamic-eid get-entity query
                                        query-one remove-triples
                                        transform-entity]]))

(def battle-data
  #:battle-data
   {:num-moment-per-turn 2
    :actors [:actor/hilda :actor/aluxes]
    :history-atom
    (atom [#:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (poison :actor/aluxes #:effect-data{:duration 1}))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (basic-attack :actor/aluxes))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (poison :actor/aluxes))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}])})

(def initial-state-triple
  [[:info/state :state/turn 0]
   [:info/state :state/desc "battle begins"]
   [:info/state :state/actors [:actor/hilda :actor/aluxes]]
   [:actor/hilda :attr/hp 560]
   [:actor/hilda :attr/mp 200]
   [:actor/aluxes :attr/hp 800]
   [:actor/aluxes :attr/mp 10]])


;; Engine
(defn reduce-effect-duration [state {:effect-data/keys [affected effect-id duration]}]
  (let [new-duration (dec duration)]
    (cond (= duration -1) state
          (= new-duration 0) (-> state
                                 (remove-triples [affected :attr/effect effect-id])
                                 (remove-triples [effect-id '_ '_]))
          :else (transform-entity state effect-id {:effect-data/duration new-duration}))))

(defmulti unleash-effect :effect-data/effect-name)

(defmethod unleash-effect :default [_] nil)

(defn reduce-effects [original-timeline event]
  (let [state (peek original-timeline)
        effects-id (query '[:find ?affected ?effect-id :where [?affected :attr/effect ?effect-id]] state)]
    (->> effects-id
         (map (fn [[affected effects-id]] [affected effects-id (get-entity state effects-id)]))
         (reduce (fn [timeline [affected effect-id effect-data]]
                   (let [state (peek timeline)
                         extra-data #:effect-data{:affected affected :effect-id effect-id :event event :state state}
                         new-moment (unleash-effect (merge effect-data extra-data))]
                     (cond-> timeline
                       (some? new-moment) (conj new-moment))))
                 original-timeline))))

(defn do-eval [ns-symbol form]
  (require ns-symbol)
  (let [user-ns (create-ns ns-symbol)]
    (binding [*ns* user-ns] (clojure.core/eval form))))

(defn basic-attack [actor target]
  (fn [state]
    (let [damage 50]
      (-> state
          (transform-entity target {:attr/hp #(- % damage)})
          (transform-entity :info/state {:state/desc (str actor " attacks " target " for " damage " damage!")})))))

(defn poison
  ([actor target] (poison actor target #:effect-data{:duration 3}))
  ([actor target {:effect-data/keys [duration]}]
   (fn [state]
     (let [manacost 30 debuff :debuff/poison
           current-poison (query-one '[:find ?eid :in $ ?target ?name
                                       :where [?target :attr/effect ?eid]
                                       [?eid :effect-data/effect-name ?name]]
                                     state target debuff)
           poison-entity (or current-poison (gen-dynamic-eid state))]
       (-> state
           (transform-entity target {:attr/mp #(- % manacost)
                                     :attr/effect poison-entity})
           (transform-entity poison-entity #:effect-data{:effect-name debuff :source actor :duration duration})
           (transform-entity :info/state {:state/desc (str actor " poisons " target " ! " target " is now poisoned!")}))))))

(defmethod unleash-effect :debuff/poison
  [{:effect-data/keys [affected event state] :as effect-data}]
  (when (= event :event/on-turn-begins)
    (let [affected-entity (get-entity state affected)
          affected-hp (:attr/hp affected-entity)
          damage (Math/floor (/ affected-hp 10))]
      (-> state
          (reduce-effect-duration effect-data)
          (transform-entity affected {:attr/hp #(- % damage)})
          (transform-entity :info/state {:state/desc (str affected " is poisoned! receives " damage " damage!")})))))

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
               new-timeline (conj [] (cond-> (transform-entity state :info/state {:state/turn inc})
                                       (empty? remaining-turns) (conj [:info/state :state/last-turn? true])))
               new-timeline (reduce-effects new-timeline :event/on-turn-begins)

               new-timeline (loop [moment-timeline new-timeline
                                   [moment & remaining-moments] moments-per-turn]
                              (let [{:moment/keys [action]} moment
                                    moment-timeline (reduce-effects moment-timeline :event/on-moment-begins)
                                    alter (do-eval model action)
                                    state (peek moment-timeline)
                                    new-moment (alter state)
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
  (add-tap #(def last-tap %))
  (add-tap #(println %))
  last-tap

  basic-attack poison

  (reduce-timeline 'engine.timeline initial-state-triple battle-data))