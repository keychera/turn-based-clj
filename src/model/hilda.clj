(ns model.hilda
  (:require [com.rpl.specter :refer [select-one setval transform]]
            [engine.timeline :refer [unleash-effect reduce-effect-duration]]))

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

(defn poison
  ([actor target] (poison actor target #:effect-data{:duration 3}))
  ([actor target {:effect-data/keys [duration]}]
   (fn [state]
     (let [manacost 30 debuff :debuff/poison]
       (->> state
            (transform [:state/entities actor :attr/mp] #(- % manacost))
            (transform [:state/entities target :attr/effect]
                       #(assoc % debuff #:effect-data{:source actor :duration duration}))
            (setval [:state/desc] (str actor " poisons " target "! " target " is now poisoned!")))))))

(defmethod unleash-effect :debuff/poison
  [{:effect-data/keys [affected event state] :as effect-data}]
  (when (= event :event/on-turn-begins)
    (let [affected-hp (select-one [:state/entities affected :attr/hp] state)
          damage (Math/floor (/ affected-hp 10))]
      (->> state
           (reduce-effect-duration effect-data)
           (transform [:state/entities affected :attr/hp] (fn [hp] (- hp damage)))
           (setval [:state/desc] (str affected " is poisoned! receives " damage " damage!"))))))

(defn charm [actor target]
  (fn [state]
    (let [manacost 80 debuff :debuff/charm duration 3]
      (->> state
           (transform [:state/entities actor :attr/mp] #(- % manacost))
           (transform [:state/entities target :attr/effect]
                      #(assoc % debuff #:effect-data{:source actor :duration duration}))
           (setval [:state/desc] (str actor " charms " target "! " target " is now charmed"))))))


(def initial-state
  #:state{:turn 0
          :desc "battle begins"
          :entities #:actor{:hilda #:attr{:hp 560 :mp 200}
                            :aluxes #:attr{:hp 800 :mp 10}}})

(defn turn-model [{:battle-data/keys [actors]}]
  (let [moment-per-turn (count actors)]
    (fn [history] [(take moment-per-turn history) (drop moment-per-turn history)])))

(def battle-data
  #:battle-data
   {:actors [:actor/hilda :actor/aluxes]
    :history-atom
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
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}])})

(comment
  #_{:clj-kondo/ignore [:duplicate-require]}
  (require '[engine.timeline :refer [reduce-timeline]])
  (add-tap #(def last-tap %))
  (add-tap #(println %))

  ;; just for removing warning
  nothing-happened basic-attack fireball magic-up poison charm
  turn-model

  (reduce-timeline 'model.hilda initial-state battle-data))