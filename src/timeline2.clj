(ns timeline2
  (:require [com.rpl.specter :refer [transform]]))

(def initial-state
  #:state{:desc "battle begins"
          :entity #:actor{:hilda #:attr{:hp 40 :mp 200}
                          :aluxes #:attr{:hp 120 :mp 10}}})

(defn basic-attack [actor target]
  (fn [state]
    (let [damage 50]
      (->> state
           (transform [:state/entity target :attr/hp] #(- % damage))))))


(defn fireball [actor target]
  (fn [state]
    (let [manacost 15 damage 50]
    (->> state
         (transform [:state/entity actor :attr/mp] #(- % manacost))
         (transform [:state/entity target :attr/hp] #(- % damage))))))

(defn magic-up [actor]
  (fn [state]
    (let [manacost 40]
      (->> state
           (transform [:state/entity actor :attr/mp] #(- % manacost))
           (transform [:state/entity actor] #(assoc % :attr/buff :buff/magic-up))))))

(def history
  (atom ['identity
         '(-> :actor/hilda (fireball :actor/aluxes))
         '(-> :actor/hilda (magic-up))
         '(-> :actor/aluxes (basic-attack :actor/hilda))]))

(defn reduce-timeline [initial-state history limit]
  (->> history
       (take (min limit (count history)))
       (reduce (fn [timeline alter-fn]
                 (let [current-timeline (last timeline)
                       alter (eval alter-fn)]
                   (conj timeline (alter current-timeline))))
               [initial-state])))

(comment
  @history
  
  (reduce-timeline initial-state @history 5)
  )