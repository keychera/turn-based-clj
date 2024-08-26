(ns timeline)

(defonce turn (atom 0))
(def initial-state
  #:state{:actors #:actor{:hilda  #:attr{:key :actor/hilda
                                         :name "A Peculiar Witch"
                                         :hp 40 :mp 200 :atk 12 :def 20
                                         :moveset #{:action/attack :spell/fireball :spell/poison}}
                          :aluxes #:attr{:key :actor/aluxes
                                         :name "Aluxes"
                                         :hp 65 :mp 25 :atk 30 :def 10
                                         :moveset #{:action/attack :action/slash}}}})

(def nothing
  (fn nothing-fn [state] #:moment{:desc  "nothing happened"
                                  :state state}))

(defn update-actor [state actor f]
  (update-in state [:state/actors (:attr/key actor)] f))

(defn attack [self-key target-key]
  (fn attack-alter-fn [state]
    (let [self (-> state :state/actors self-key)
          target (-> state :state/actors target-key)
          damage (max (abs (/ (- (:attr/atk self) (:attr/def target)) 2)) 0)]
      #:moment{:desc  (str (:attr/name self) " attacked " (:attr/name target) " for " damage " damage!")
               :state (-> state (update-actor target #(update % :attr/hp - damage)))})))

(defn fireball [self-key target-key]
  (fn fireball-alter-fn [state] 
    (let [self (-> state :state/actors self-key)
          target (-> state :state/actors target-key)
          damage 25] 
      #:moment{:desc  (str (:attr/name self) " casted fireball towards " (:attr/name target) " for " damage " damage!")
               :state (-> state
                          (update-actor target #(update % :attr/hp - damage))
                          (update-actor self #(update % :attr/mp - 10)))})))

(def timeline
  (atom [nothing
         (-> :actor/hilda  (fireball :actor/aluxes))
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
