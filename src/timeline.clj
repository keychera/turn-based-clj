(ns timeline)

(defonce turn (atom 0))
(def initial-state
  #:state{:actors #:actor{:hilda  {:hp 40 :mp 200 :atk 12 :def 20}
                          :aluxes {:hp 65 :mp 25 :atk 30 :def 10}}})

(def nothing
  (fn nothing-fn [state] {:desc  "nothing happened"
                          :state state}))

(defn attack-damage-calc [actor target]
  (max (abs (/ (- (:atk actor) (:def target)) 2)) 0))

(defn attack [actor-key target-key]
  (fn attack-alter-fn [state]
    (let [actor (-> state :state/actors actor-key)
          target (-> state :state/actors target-key)
          damage (attack-damage-calc actor target)] 
      {:desc  (str actor-key " attacks " target-key " for " damage " damage!")
       :state (update-in state [:state/actors target-key] #(update % :hp - damage))})))

(def history
  (atom [nothing
         (-> :actor/hilda  (attack :actor/aluxes))
         (-> :actor/aluxes (attack :actor/hilda))
        ;;  (-> :actor/hilda  (cast :magic/fireball {:to :actor/aluxes}))
         nothing]))

(defn reduce-timeline [limit]
  (let [history @history]
    (->> history
         (take (min limit (count history)))
         (reduce (fn [timeline alter-fn]
                   (let [{:keys [state]} (last timeline)]
                     (conj timeline (alter-fn state))))
                 [{:desc "battle begins"
                   :state initial-state}]))))


(comment
  (add-tap #(def last-tap %))
  (reduce-timeline 2))
