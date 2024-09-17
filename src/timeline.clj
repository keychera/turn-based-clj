(ns timeline
  (:require [com.rpl.specter :as sp :refer [select select-one setval transform]]))

(def initial-state
  #:state{:desc "battle begins"
          :entities #:actor{:hilda #:attr{:hp 560 :mp 200}
                            :aluxes #:attr{:hp 800 :mp 10}}})

;; Actions 

(defn nothing-happened [state]
  (->> state (setval [:state/desc] "nothing happened!")))

(defn basic-attack [actor target]
  (fn [state]
    (let [damage 50]
      (->> state
           (transform [:state/entities target :attr/hp] #(- % damage))
           (setval [:state/desc] (str actor " attacks " target " for " damage " damage!"))))))

(defn fireball [actor target]
  (fn [state]
    (let [manacost 15 damage 50]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities target :attr/hp] #(- % damage))
           (setval [:state/desc] (str actor " cast fireball towards " target " for " damage " damage!"))))))

(defn magic-up [actor]
  (fn [state]
    (let [manacost 40 buff :buff/magic-up duration 3]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities actor :attr/effect]
                      #(assoc % buff #:effect-data{:source actor :duration duration}))
           (setval [:state/desc] (str actor " mafic attack is buffed!"))))))

(defn poison [actor target]
  (fn [state]
    (let [manacost 30 damage 5 debuff :debuff/poison duration -1]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities target :attr/hp] #(- % damage))
           (transform [:state/entities target :attr/effect]
                      #(assoc % debuff #:effect-data{:source actor :duration duration}))
           (setval [:state/desc] (str actor " poisons " target " for " damage " damage! " target " is now poisoned!"))))))


(defn charm [actor target]
  (fn [state]
    (let [manacost 80 debuff :debuff/charm duration 3]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities target :attr/effect]
                      #(assoc % debuff #:effect-data{:source actor :duration duration}))
           (setval [:state/desc] (str actor " charms " target "! " target " is now charmed"))))))

;; Effects

(defmulti unleash-effect :effect-data/effect-name)

(defmethod unleash-effect :debuff/poison
  [{:effect-data/keys [afflicted state]}]
  (let [afflicted-hp (select-one [:state/entities afflicted :attr/hp] state)
        _ (tap> afflicted-hp)
        damage (Math/floor (/ afflicted-hp 10))]
    (->> state
         (transform [:state/entities afflicted :attr/hp] (fn [hp] (- hp damage)))
         (setval [:state/desc] (str afflicted " is poisoned! receives " damage " damage!")))))

(defmethod unleash-effect :default [{:effect-data/keys [effect-name state]}]
  (->> state (setval [:state/desc] (str "notthing happened for " effect-name))))

;; History

(def history
  (atom ['nothing-happened
         '(-> :actor/hilda (poison :actor/aluxes))
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         '(-> :actor/hilda (charm :actor/aluxes))
         'nothing-happened
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         '(-> :actor/hilda (magic-up))
         'nothing-happened
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         '(-> :actor/hilda (fireball :actor/aluxes))
         '(-> :actor/aluxes (basic-attack :actor/hilda))
         'nothing-happened
         'nothing-happened
         'nothing-happened]))

;; Engine

(def turn (atom 0))

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
        new-duration (dec duration)
        state-after-effect (unleash-effect (assoc effect-data :effect-data/state state))]
    (cond
      (= duration -1)    state-after-effect
      (= new-duration 0) (->> state-after-effect (setval [:state/entities afflicted :attr/effect effect-name] sp/NONE))
      :else              (->> state-after-effect (setval [:state/entities afflicted :attr/effect effect-name :effect-data/duration] new-duration)))))

(defn reduce-effects [original-timeline]
  (let [state (peek original-timeline)
        all-afflicted (select [:state/entities sp/ALL (sp/selected? (fn [[_ attr]] (:attr/effect attr)))] state)
        afflictions (entities->effect-data all-afflicted)]
    (->> afflictions
         (reduce (fn [timeline effect-data]
                   (let [state (peek timeline)
                         new-history (apply-effect effect-data state)]
                     (-> timeline (conj new-history))))
                 original-timeline))))

(defn do-eval [namespace-sym form]
  (let [user-ns (create-ns namespace-sym)]
    (binding [*ns* user-ns] (clojure.core/eval form))))

(defn reduce-timeline
  ([initial-state history]
   (reduce-timeline initial-state history (count history)))
  ([initial-state history limit]
   (->> history
        (take (min limit (count history)))
        (reduce (fn [timeline alter-fn]
                  (let [alter (do-eval 'timeline2 alter-fn)
                        state (peek timeline)
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