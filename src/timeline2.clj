(ns timeline2
  (:require [com.rpl.specter :as sp :refer [select setval transform]]))

(def initial-state
  #:state{:desc "battle begins"
          :entities #:actor{:hilda #:attr{:hp 560 :mp 200}
                            :aluxes #:attr{:hp 800 :mp 10}}})

(defn basic-attack [actor target]
  (fn [state]
    (let [damage 50]
      (->> state
           (transform [:state/entities target :attr/hp] #(- % damage))))))


(defn fireball [actor target]
  (fn [state]
    (let [manacost 15 damage 50]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities target :attr/hp] #(- % damage))))))

(defn magic-up [actor]
  (fn [state]
    (let [manacost 40 buff :effect/poison duration 3]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities actor :attr/effect]
                      #(assoc % buff #:effect-data{:source actor :duration duration}))))))

(defn poison [actor target]
  (fn [state]
    (let [manacost 30 damage 5 debuff :effect/poison duration 3]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities target :attr/hp] #(- % damage))
           (transform [:state/entities target :attr/effect]
                      #(assoc % debuff #:effect-data{:source actor :duration duration}))))))


(defn charm [actor target]
  (fn [state]
    (let [manacost 80 debuff :effect/charm duration 3]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities target :attr/effect]
                      #(assoc % debuff #:effect-data{:source actor :duration duration}))))))



(def history
  (atom ['identity
         '(-> :actor/hilda (poison :actor/aluxes))
         'identity
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         'identity
         '(-> :actor/hilda (charm :actor/aluxes))
         'identity
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         'identity
         '(-> :actor/hilda (magic-up))
         'identity
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         'identity
         '(-> :actor/hilda (fireball :actor/aluxes))
         'identity
         '(-> :actor/aluxes (basic-attack :actor/hilda))]))

(defn entities->effect-data [entities]
  (->> entities
       (mapcat (fn [[afflicted attr]]
                 (->> (:attr/effect attr)
                      (map (fn [[effect-name effect-data]]
                             (assoc effect-data
                                    :effect-data/effect-name effect-name
                                    :effect-data/afflicted afflicted))))))))

(defn apply-effect [effect-data state]
  (let [{:effect-data/keys [afflicted effect-name duration]} effect-data
        new-duration (min duration (dec duration))]
    (case new-duration
      -1 state
      0 (->> state (setval [:state/entities afflicted :attr/effect effect-name] sp/NONE))
      (->> state (setval [:state/entities afflicted :attr/effect effect-name :effect-data/duration] new-duration)))))

(defn reduce-effects [timeline]
  (let [state   (last timeline)
        all-afflicted (select [:state/entities sp/ALL (sp/selected? (fn [[_ attr]] (:attr/effect attr)))] state)
        afflictions (entities->effect-data all-afflicted)]
    (->> afflictions
         (reduce (fn [timeline effect-data]
                   (let [state (last timeline)
                         new-history (apply-effect effect-data state)]
                     (-> timeline (conj new-history))))
                 timeline))))

(defn reduce-timeline
  ([initial-state history]
   (reduce-timeline initial-state history (count history)))
  ([initial-state history limit]
   (->> history
        (take (min limit (count history)))
        (reduce (fn [timeline alter-fn]
                  (let [alter (eval alter-fn)
                        state (last timeline)
                        new-history (alter state)
                        new-turn (inc (or (:state/turn state) 0))]
                    (-> timeline
                        (conj (-> new-history (assoc :state/turn new-turn)))
                        (reduce-effects))))
                [initial-state]))))

(comment
  @history

  (add-tap #(def last-tap %))
  (add-tap #(println %))
  last-tap
  (tap> "hello")

  (reduce-timeline initial-state @history 5))