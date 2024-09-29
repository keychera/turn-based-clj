(ns model.hilda
  (:require [engine.timeline :refer [reduce-effect-duration unleash-effect]]
            [engine.triplestore :refer [gen-dynamic-eid get-attr
                                        transform-entity]]
            [pod.huahaiy.datalevin :as d]))

(defn basic-attack [actor target]
  (fn basic-attack [moment]
    (let [damage 50]
      (-> moment
          (transform-entity target {:attr/hp #(- % damage)})
          (transform-entity :info/moment {:moment/desc (str actor " attacks " target " for " damage " damage!")})))))


(defn fireball [actor target]
  (fn fireball [moment]
    (let [manacost 15 damage 50]
      (-> moment
          (transform-entity actor {:attr/mp #(- % manacost)})
          (transform-entity target {:attr/hp #(- % damage)})
          (transform-entity :info/moment {:moment/desc (str actor " cast fireball towards " target " for " damage " damage!")})))))

(defn magic-up [actor]
  (fn magic-up [moment]
    (let [manacost 40 effect-name :buff/magic-up duration 4
          current-effect (d/q '[:find ?eid . :in $ ?target ?name
                                :where [?target :attr/effects ?eid]
                                [?eid :effect-data/effect-name ?name]]
                              moment actor effect-name)
          effect-entity (or current-effect (gen-dynamic-eid moment))]
      (-> moment
          (transform-entity actor {:attr/mp #(- % manacost)
                                   :attr/effects [:add effect-entity]})
          (transform-entity effect-entity #:effect-data{:effect-name effect-name :duration duration})
          (transform-entity :info/moment {:moment/desc (str actor " magic attack is buffed!")})))))

(defn poison
  ([actor target] (poison actor target #:effect-data{:duration 3}))
  ([actor target {:effect-data/keys [duration]}]
   (fn poison [moment]
     (let [manacost 30 effect-name :debuff/poison
           current-effect (d/q '[:find ?eid . :in $ ?target ?name
                                 :where [?target :attr/effects ?eid]
                                 [?eid :effect-data/effect-name ?name]]
                               moment target effect-name)
           effect-entity (or current-effect (gen-dynamic-eid moment))]
       (-> moment
           (transform-entity actor {:attr/mp #(- % manacost)})
           (transform-entity target {:attr/effects [:add effect-entity]})
           (transform-entity effect-entity #:effect-data{:effect-name effect-name :source actor :duration duration})
           (transform-entity :info/moment {:moment/desc (str actor " poisons " target " ! " target " is now poisoned!")}))))))

(defmethod unleash-effect [:debuff/poison :event/on-turn-begins]
  [{:effect-data/keys [affected moment] :as effect-data}]
  (let [affected-hp (get-attr moment affected :attr/hp)
        damage      (Math/floor (/ affected-hp 10))]
    (-> moment
        (reduce-effect-duration effect-data)
        (transform-entity affected {:attr/hp #(- % damage)})
        (transform-entity :info/moment {:moment/desc (str affected " is poisoned! receives " damage " damage!")}))))

(defn charm [actor target]
  (fn charm [moment]
    (let [manacost 80 effect-name :debuff/charm duration 3
          current-effect (d/q '[:find ?eid . :in $ ?target ?name
                                :where [?target :attr/effects ?eid]
                                [?eid :effect-data/effect-name ?name]]
                              moment target effect-name)
          effect-entity (or current-effect (gen-dynamic-eid moment))]
      (-> moment
          (transform-entity actor {:attr/mp #(- % manacost)})
          (transform-entity target {:attr/effects [:add effect-entity]})
          (transform-entity effect-entity #:effect-data{:effect-name effect-name :source actor :duration duration})
          (transform-entity :info/moment {:moment/desc (str actor " charms " target "! " target " is now charmed")})))))


(def initial-moment
  [[:info/timeline :timeline/turn 0]
   [:info/timeline :timeline/actors :actor/hilda]
   [:info/timeline :timeline/actors :actor/aluxes]
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
  basic-attack fireball magic-up poison charm
  turn-model

  (reduce-timeline 'model.hilda initial-moment battle-data 1))