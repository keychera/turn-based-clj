(ns timeline)

(defonce turn (atom 0))
(def initial-state
  #:state{:actors #:actor{:hilda  #:attr{:hp 40 :mp 200 :atk 12 :def 20
                                         :moveset {}}
                          :aluxes #:attr{:hp 65 :mp 25 :atk 30 :def 10}}})

(def nothing
  (fn nothing-fn [state] #:moment{:desc  "nothing happened"
                                  :state state}))

(defn attack-damage-calc [actor target]
  (max (abs (/ (- (:attr/atk actor) (:attr/def target)) 2)) 0))

(defn attack [actor-key target-key]
  (fn attack-alter-fn [state]
    (let [actor (-> state :state/actors actor-key)
          target (-> state :state/actors target-key)
          _ (tap> ["what" state actor target])
          damage (attack-damage-calc actor target)]
      #:moment{:desc  (str actor-key " attacks " target-key " for " damage " damage!")
               :state (update-in state [:state/actors target-key] #(update % :attr/hp - damage))})))

(def timeline
  (atom [nothing
         (-> :actor/hilda  (attack :actor/aluxes))
         (-> :actor/aluxes (attack :actor/hilda))
        ;;  (-> :actor/hilda  (cast :magic/fireball {:to :actor/aluxes}))
         nothing]))

(defn reduce-timeline [limit]
  (let [timeline @timeline]
    (->> timeline
         (take (min limit (count timeline)))
         (reduce (fn [timeline alter-fn]
                   (let [{:moment/keys [state]} (last timeline)]
                     (conj timeline (alter-fn state))))
                 [#:moment{:desc  "battle begins"
                           :state initial-state}]))))

(defn history []
  (reduce-timeline (count @timeline)))

(comment
  (add-tap #(def last-tap %))
  last-tap

  (reduce-timeline 2)
  (history))
