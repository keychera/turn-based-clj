(ns model.topaz
  (:require [com.rpl.specter :as sp]
            [engine2.timeline :refer [effect-on entity]]))

(defn basic-attack [moment {:move.attr/keys [actor target]}]
  (let [damage 50]
    (->> moment
         (sp/setval    [:moment.attr/desc] (str actor " attacks " target " for " damage " damage!"))
         (sp/transform [(entity target)]
                       #(update % :actor.attr/hp - damage)))))

(defn fireball [moment {:move.attr/keys [actor target]}]
  (let [mp-cost 15 damage 50]
    (->> moment
         (sp/setval    [:moment.attr/desc] (str actor " cast fireball towards " target " for " damage " damage!"))
         (sp/transform [(entity target)]
                       (comp #(update % :actor.attr/mp - mp-cost)
                             #(update % :actor.attr/hp - damage))))))

(defn poison
  [moment {:move.attr/keys [actor target duration] :or {duration 3}}]
  (let [mp-cost 30 effect-name :debuff/poison
        actor-entity (sp/select-one [(entity :char/hilda)] moment)]
    (->> moment
         (sp/setval    [:moment.attr/desc] (str actor " poisons " target " ! " target " is now poisoned!"))
         (sp/transform [(entity actor)]
                       #(update % :actor.attr/mp - mp-cost))
         (sp/setval    [(effect-on target :debuff/poison)]
                       #:effect.attr{:effect-name effect-name
                                     :source (:db/id actor-entity)
                                     :duration duration}))))

(def debuff-poison
  #:rule
   {:rule-name       :debuff/poison
    :activation
    '[:find [?affected ?source]
      :where
      [:info/moment :moment/event :event/on-moment-begins]
      [:info/moment :moment/whose ?affected]
      [?affected :attr/effects ?eff-id]
      [?eff-id :effect-data/effect-name :debuff/poison]
      [?eff-id :effect-data/source ?source]
      [?eff-id :effect-data/duration ?duration]]
    :rule-fn
    (fn [moment [?affected ?source]]
      (let [affected-entity (sp/select-one [(entity ?affected)] moment)
            source-entity   (sp/select-one [(entity ?source)] moment)
            affected-hp     (:attr/hp affected-entity)
            damage          (Math/floor (/ affected-hp 10))]
        (-> moment
            (sp/transform [(entity ?affected)]
                          #(update % :actor.attr/hp - damage))
            (sp/setval    [:moment.attr/desc] (str ?affected " is poisoned! receives " damage " damage!"))
            (sp/transform [(effect-on ?affected :debuff/poison) :effect-data/duration] dec)
            (sp/setval    [:moment.attr/events]
                          [#:event.attr{:event-name :debuff/poison
                                        :damage      damage
                                        :affected   (:db/id source-entity)
                                        :source     (:db/id source-entity)}]))))})

(def world
  #:world
   {:moves   {:move/basic-attack 'model.topaz/basic-attack
              :move/fireball     'model.topaz/fireball
              :move/poison       'model.topaz/poison}
    :effects [debuff-poison]
    :initial #:moment.attr
              {:epoch    0
               :entities [#:actor.attr
                           {:db/id "hilda"
                            :name  :char/hilda
                            :hp    700
                            :mp    400}
                          #:actor.attr
                           {:name    :char/aluxes
                            :hp      1000
                            :mp      45
                            :effects [#:effect.attr
                                       {:effect-name :debuff/poison
                                        :source      "hilda"
                                        :duration    1}]}]}})

(def history
  [#:action.attr
    {:actor    :char/hilda
     :act-expr [:move/poison #:move.attr{:target :char/aluxes}]}
   #:action.attr
    {:actor    :char/aluxes
     :act-expr [:move/basic-attack #:move.attr{:target :char/hilda}]}
   #:action.attr
    {:actor    :char/hilda
     :act-expr [:move/fireball #:move.attr{:target :char/aluxes}]}])

(comment

  ;; just for removing warning 
  basic-attack fireball poison)