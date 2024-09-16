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
                      #(assoc % buff #:effect-info{:source actor :duration duration}))))))

(defn poison [actor target]
  (fn [state]
    (let [manacost 30 damage 5 debuff :effect/poison duration 3]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities target :attr/hp] #(- % damage))
           (transform [:state/entities target :attr/effect]
                      #(assoc % debuff #:effect-info{:source actor :duration duration}))))))


(defn charm [actor target]
  (fn [state]
    (let [manacost 80 debuff :effect/charm duration 3]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities target :attr/effect]
                      #(assoc % debuff #:effect-info{:source actor :duration duration}))))))



(def history
  (atom ['identity
         '(-> :actor/hilda (poison :actor/aluxes))
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         '(-> :actor/hilda (charm :actor/aluxes))
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         '(-> :actor/hilda (magic-up))
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         '(-> :actor/hilda (fireball :actor/aluxes))
         '(-> :actor/aluxes (basic-attack :actor/hilda))]))

(defn reduce-effects [timeline]
  (let [state   (last timeline)
        all-afflicted (select [:state/entities sp/ALL (sp/selected? (fn [[_ attr]] (:attr/effect attr)))] state)
        ;; TODO flatten instead of nested reduce  I think
        afflictions (->> all-afflicted (map (fn [[afflicted attr]] [afflicted (:attr/effect attr)])))]
    (->> afflictions
         (reduce (fn [timeline [afflicted effects]]
                   (->> effects
                        (reduce (fn [timeline [name {:effect-info/keys [duration]} :as effect]]
                                  (tap> [timeline afflicted name effect])
                                  (let [new-duration (dec duration)
                                        state (last timeline)
                                        new-history (->> state
                                                         ;; TODO each effect here
                                                         ;; TODO remove on duration 0
                                                         (setval [:state/entities afflicted :attr/effect name :effect-info/duration] new-duration))]
                                    (-> timeline (conj new-history))))
                                timeline)))
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
                        new-turn (inc (or (:state/turn state) 0))
                        _ (tap> [:turn new-turn])]
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