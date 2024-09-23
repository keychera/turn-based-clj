(ns model.hilda
  (:require [engine.timeline :refer [reduce-effect-duration unleash-effect]]
            [engine.triplestore :refer [gen-dynamic-eid get-attr query-one
                                        transform-entity]]))

(defn nothing-happened [state]
  (-> state (transform-entity :info/state {:state/desc "nothing happened!"})))

(defn basic-attack [actor target]
  (fn basic-attack [state]
    (let [damage 50]
      (-> state
          (transform-entity target {:attr/hp #(- % damage)})
          (transform-entity :info/state {:state/desc (str actor " attacks " target " for " damage " damage!")})))))


(defn fireball [actor target]
  (fn fireball [state]
    (let [manacost 15 damage 50]
      (-> state
          (transform-entity actor {:attr/mp #(- % manacost)})
          (transform-entity target {:attr/hp #(- % damage)})
          (transform-entity :info/state {:state/desc (str actor " cast fireball towards " target " for " damage " damage!")})))))

(defn magic-up [actor]
  (fn magic-up [state]
    #_{:clj-kondo/ignore [:unused-binding]}
    (let [manacost 40 effect-name :buff/magic-up duration 4
          current-effect (query-one '[:find ?eid :in $ ?target ?name
                                      :where [?target :attr/effect ?eid]
                                      [?eid :effect-data/effect-name ?name]]
                                    state actor effect-name)
          effect-entity (or current-effect (gen-dynamic-eid state))]
      (-> state
          (transform-entity actor {:attr/mp #(- % manacost)})
          (transform-entity actor {:attr/effect [:add effect-entity]})
          (transform-entity effect-entity #:effect-data{:effect-name effect-name :duration duration}) 
          (transform-entity :info/state {:state/desc (str actor " magic attack is buffed!")})))))

(defn poison
  ([actor target] (poison actor target #:effect-data{:duration 3}))
  ([actor target {:effect-data/keys [duration]}]
   (fn poison [state]
     (let [manacost 30 effect-name :debuff/poison
           current-effect (query-one '[:find ?eid :in $ ?target ?name
                                       :where [?target :attr/effect ?eid]
                                       [?eid :effect-data/effect-name ?name]]
                                     state target effect-name)
           effect-entity (or current-effect (gen-dynamic-eid state))]
       (-> state
           (transform-entity actor {:attr/mp #(- % manacost)})
           (transform-entity target {:attr/effect [:add effect-entity]})
           (transform-entity effect-entity #:effect-data{:effect-name effect-name :source actor :duration duration})
           (transform-entity :info/state {:state/desc (str actor " poisons " target " ! " target " is now poisoned!")}))))))

(defmethod unleash-effect :debuff/poison
  [{:effect-data/keys [affected event state] :as effect-data}]
  (when (= event :event/on-turn-begins)
    (let [affected-hp (get-attr state affected :attr/hp)
          damage (Math/floor (/ affected-hp 10))]
      (-> state
          (reduce-effect-duration effect-data)
          (transform-entity affected {:attr/hp #(- % damage)})
          (transform-entity :info/state {:state/desc (str affected " is poisoned! receives " damage " damage!")})))))

(defn charm [actor target]
  (fn charm [state]
    #_{:clj-kondo/ignore [:unused-binding]}
    (let [manacost 80 effect-name :debuff/charm duration 3
          current-effect (query-one '[:find ?eid :in $ ?target ?name
                                      :where [?target :attr/effect ?eid]
                                      [?eid :effect-data/effect-name ?name]]
                                    state target effect-name)
          effect-entity (or current-effect (gen-dynamic-eid state))]
      (-> state
          (transform-entity actor {:attr/mp #(- % manacost)})
          (transform-entity target {:attr/effect [:add effect-entity]})
          (transform-entity effect-entity #:effect-data{:effect-name effect-name :source actor :duration duration})
          (transform-entity :info/state {:state/desc (str actor " charms " target "! " target " is now charmed")})))))


(def initial-state
  [[:info/state :state/turn 0]
   [:info/state :state/desc "battle begins"]
   [:info/state :state/actors [:actor/hilda :actor/aluxes]]
   [:actor/hilda :attr/hp 560]
   [:actor/hilda :attr/mp 200]
   [:actor/aluxes :attr/hp 800]
   [:actor/aluxes :attr/mp 10]])

(defn turn-model [{:battle-data/keys [actors]}]
  (let [moment-per-turn (count actors)]
    (fn [history] [(take moment-per-turn history) (drop moment-per-turn history)])))

(def battle-data
  #:battle-data
   {:num-moment-per-turn 2
    :actors [:actor/hilda :actor/aluxes]
    :history-atom
    (atom [#:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (poison :actor/aluxes))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (charm :actor/aluxes))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (magic-up))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (fireball :actor/aluxes))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (magic-up))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (fireball :actor/aluxes))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (fireball :actor/aluxes))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}])})

(comment
  #_{:clj-kondo/ignore [:duplicate-require]}
  (require '[engine.timeline :refer [reduce-timeline]])
  (add-tap #(def last-tap %))
  (add-tap #(println %))

  ;; just for removing warning
  nothing-happened basic-attack fireball magic-up poison charm
  turn-model

  (reduce-timeline 'model.hilda initial-state battle-data 1))