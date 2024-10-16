(ns model.topaz
  (:require [com.rpl.specter :as sp]
            [engine2.timeline :refer [entity entity-id on-effect]]))

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
        actor-entity (sp/select-one [(entity actor)] moment)]
    (->> moment
         (sp/setval    [:moment.attr/desc] (str actor " poisons " target " ! " target " is now poisoned!"))
         (sp/transform [(entity actor)]
                       #(update % :actor.attr/mp - mp-cost))
         (sp/setval    [(entity target) (on-effect :debuff/poison)]
                       #:effect.attr{:effect-name effect-name
                                     :source (:db/id actor-entity)
                                     :duration duration}))))

(def debuff-poison
  #:rule
   {:rule-name  :debuff/poison
    :timing     :timing/before-action
    :activation '[:find [?affected-id ?source-id]
                  :where
                  [?current-moment :moment.attr/entities ?affected-id]
                  [?affected-id :actor.attr/effects ?eff-id]
                  [?eff-id :effect.attr/effect-name :debuff/poison]
                  [?eff-id :effect.attr/source ?source-id]]
    :rule-fn    (fn [moment [?affected-id ?source-id]]
                  (let [affected-entity (sp/select-one [(entity-id ?affected-id)] moment)
                        source-entity   (sp/select-one [(entity-id ?source-id)] moment)
                        affected-hp     (:actor.attr/hp affected-entity)
                        damage          (Math/floor (/ affected-hp 10))]
                    (->> moment
                         (sp/transform [(entity-id ?affected-id)]
                                       #(update % :actor.attr/hp - damage))
                         (sp/setval    [:moment.attr/desc] (str (:actor.attr/name affected-entity) " is poisoned! receives " damage " damage!"))
                         (sp/transform [(entity-id ?affected-id) (on-effect :debuff/poison) :effect.attr/duration] dec)
                         (sp/transform [:moment.attr/events]
                                       #(conj % #:event.attr{:event-name  :debuff/poison
                                                             :damage      damage
                                                             :affected-id (select-keys affected-entity [:db/id])
                                                             :source-id   (select-keys source-entity [:db/id])})))))})

(def world
  #:world
   {:moves   {:move/basic-attack 'model.topaz/basic-attack
              :move/fireball     'model.topaz/fireball
              :move/poison       'model.topaz/poison}
    :rules   [debuff-poison]
    :initial #:moment.attr
              {:epoch    0
               :entities [#:actor.attr
                           {:db/id "hilda"
                            :name  :char/hilda
                            :hp    700
                            :mp    400
                            :effects []}
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