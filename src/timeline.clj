(ns timeline
  (:require [com.rpl.specter :as sp :refer [select select-one setval transform]]))

(def initial-state
  #:state{:moment 0
          :desc "battle begins"
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
           (setval [:state/desc] (str actor " magic attack is buffed!"))))))

(defn poison [actor target]
  (fn [state]
    (let [manacost 30 damage 5 debuff :debuff/poison duration 3]
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

(defn reduce-duration [{:effect-data/keys [duration affected effect-name]} state]
  (let [new-duration (dec duration)]
    (cond
      (= new-duration 0) (->> state (setval [:state/entities affected :attr/effect effect-name] sp/NONE))
      :else              (->> state (setval [:state/entities affected :attr/effect effect-name :effect-data/duration] new-duration)))))

(defmulti unleash-effect :effect-data/effect-name)

(defmethod unleash-effect :debuff/poison
  [{:effect-data/keys [affected whose event state] :as effect-data}]
  (when (and (= event :event/on-moment-begins)
             (= affected whose))
    (let [affected-hp (select-one [:state/entities affected :attr/hp] state)
          damage (Math/floor (/ affected-hp 10))]
      (->> state
           (reduce-duration effect-data)
           (transform [:state/entities affected :attr/hp] (fn [hp] (- hp damage)))
           (setval [:state/desc] (str affected " is poisoned! receives " damage " damage!"))))))

(defmethod unleash-effect :default [_] nil)

;; History

(def history
  (atom [#:moment{:whose  :actor/hilda
                  :action '(-> :actor/hilda (poison :actor/aluxes))}
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
                  :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

         #:moment{:whose  :actor/hilda
                  :action '(-> :actor/hilda (fireball :actor/aluxes))}
         #:moment{:whose  :actor/aluxes
                  :action '(-> :actor/aluxes (basic-attack :actor/hilda))}]))

;; Engine

(def moment (atom 0))

(defn entities->effect-data [entities]
  (->> entities
       (mapcat (fn [[affected attr]]
                 (->> (:attr/effect attr)
                      (map (fn [[effect-name effect-data]]
                             (assoc effect-data
                                    :effect-data/effect-name effect-name
                                    :effect-data/affected affected))))))))

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
  ([initial-state history]
   (reduce-timeline initial-state history (count history)))
  ([initial-state history limit]
   (->> history
        (take (min limit (count history)))
        (reduce (fn [timeline {:moment/keys [whose action] :as moment}]
                  (let [alter (do-eval 'timeline action)
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
  @history

  (add-tap #(def last-tap %))
  (add-tap #(println %))
  last-tap
  (tap> "hello")

  (reduce-timeline initial-state @history 5))