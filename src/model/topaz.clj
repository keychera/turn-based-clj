(ns model.topaz
  (:require [com.rpl.specter :as sp]
            [engine2.timeline :refer [entity entity-id on-effect
                                      on-or-add-effect]]))

(defn basic-attack [moment {:action.attr/keys [actor-name target-name]}]
  (let [damage 50]
    (->> moment
         (sp/transform [(entity target-name)]
                       #(update % :actor.attr/hp - damage))
         (sp/transform [:moment.attr/facts]
                       #(conj % #:fact{:desc (str actor-name " attacks " target-name " for " damage " damage!")})))))

(defn fireball [moment {:action.attr/keys [actor-name target-name]}]
  (let [mp-cost 15 damage 50]
    (->> moment
         (sp/transform [(entity target-name)]
                       (comp #(update % :actor.attr/mp - mp-cost)
                             #(update % :actor.attr/hp - damage)))
         (sp/transform [:moment.attr/facts]
                       #(conj % #:fact{:desc (str actor-name " cast fireball towards " target-name " for " damage " damage!")})))))

(defn poison
  [moment {:action.attr/keys [actor-name target-name duration] :or {duration 3}}]
  (let [mp-cost 30 effect-name :debuff/poison]
    (->> moment
         (sp/transform [(entity actor-name)]
                       #(update % :actor.attr/mp - mp-cost))
         (sp/setval    [(entity target-name) (on-or-add-effect :debuff/poison actor-name)]
                       #:effect.attr{:effect-name effect-name
                                     :source-name actor-name
                                     :duration duration})
         (sp/transform [:moment.attr/facts]
                       #(conj % #:fact{:desc (str actor-name " poisons " target-name " ! " target-name " is now poisoned!")})))))

(def debuff-poison
  #:rule
   {:rule-name  :debuff/poison
    :activation '[:find [?affected-id ?source-name]
                  :where
                  [(= ?s.timing :timing/before-action)]
                  [(= ?s.who-acts ?actor-name)]
                  [?s.current-moment :moment.attr/entities ?affected-id]
                  [?affected-id :actor.attr/actor-name ?actor-name]
                  [?affected-id :actor.attr/effects ?eff-id]
                  [?eff-id :effect.attr/effect-name :debuff/poison]
                  [?eff-id :effect.attr/source-name ?source-name]]
    :rule-fn    (fn [moment [?affected-id ?source-name]]
                  (let [affected-entity (sp/select-one [(entity-id ?affected-id)] moment)
                        affected-hp     (:actor.attr/hp affected-entity)
                        affected-name   (:actor.attr/actor-name affected-entity)
                        damage          (Math/floor (/ affected-hp 10))]
                    (->> moment
                         (sp/transform [(entity-id ?affected-id)]
                                       #(update % :actor.attr/hp - damage))
                         (sp/transform [(entity-id ?affected-id) (on-effect :debuff/poison ?source-name) :effect.attr/duration] dec)
                         (sp/transform [:moment.attr/facts]
                                       #(conj % #:fact{:desc          (str affected-name " is poisoned! receives " damage " damage!")
                                                       :event         :event/poison-unleashed
                                                       :damage        damage
                                                       :affected-name affected-name
                                                       :source-name   ?source-name})))))})

(def talent-clara
  #:rule
   {:rule-name       :talent/clara
    :rules-to-inject '[[(talent-clara? ?s.current-moment ?id-with-talent ?name-with-talent)
                        [?s.current-moment :moment.attr/entities ?id-with-talent]
                        [?id-with-talent :actor.attr/effects ?eff-id]
                        [?id-with-talent :actor.attr/actor-name ?name-with-talent]
                        [?eff-id :effect.attr/effect-name :talent/clara]]
                       [(targetted? ?s.current-moment ?attacker-id ?attacker-name ?targetted-name ?action-name)
                        [?s.current-moment :moment.attr/facts ?fact-id]
                        [?s.current-moment :moment.attr/entities ?attacker-id]
                        [?attacker-id :actor.attr/actor-name ?attacker-name]
                        [?fact-id :action.attr/actor-name ?attacker-name]
                        [?fact-id :action.attr/action-name ?action-name]
                        [?fact-id :action.attr/target-name ?targetted-name]]]
    :activation      '[:find [?targetted-id ?attacker-id]
                       :where
                       [(= ?s.timing :timing/after-action)]
                       (talent-clara? ?s.current-moment ?targetted-id ?targetted-name)
                       (targetted? ?s.current-moment ?attacker-id ?attacker-name ?targetted-name ?action-name)
                       [(= ?action-name :move/fireball)]
                       [(= ?s.who-acts ?attacker-name)]]
    :rule-fn         (fn [moment [?targetted-id ?attacker-id]]
                       (let [targeted-entity (sp/select-one [(entity-id ?targetted-id)] moment)
                             attacker-entity (sp/select-one [(entity-id ?attacker-id)] moment)
                             attacker-hp     (:actor.attr/hp targeted-entity)
                             _               (prn "countering..." targeted-entity attacker-entity)
                             damage          (Math/floor (/ attacker-hp 10))]
                         (->> moment
                              (sp/transform [(entity-id ?attacker-id)]
                                            #(update % :actor.attr/hp - damage))
                              (sp/transform [:moment.attr/facts]
                                            #(conj % #:fact{:desc        (str (:actor.attr/actor-name targeted-entity) " countered " (:actor.attr/actor-name attacker-entity) "'s attack with " damage " damage!")
                                                            :event       :event/clara-talent-unleashed
                                                            :damage      damage
                                                            :affected-id (select-keys targeted-entity [:db/id])
                                                            :source-id   (select-keys attacker-entity [:db/id])})))))})

(def world
  #:world
   {:actions {:move/basic-attack 'model.topaz/basic-attack
              :move/fireball     'model.topaz/fireball
              :move/poison       'model.topaz/poison}
    :rules   [debuff-poison talent-clara]
    :initial #:moment.attr
              {:epoch    0
               :entities [#:actor.attr
                           {:db/id      "hilda"
                            :actor-name :char/hilda
                            :hp         700
                            :mp         400
                            :effects    []}
                          #:actor.attr
                           {:actor-name :char/aluxes
                            :hp         1000
                            :mp         45
                            :effects    [#:effect.attr {:effect-name :talent/clara}
                                         #:effect.attr
                                          {:effect-name :debuff/poison
                                           :source-name :char/hilda
                                           :duration    1}]}]}})

(def history
  [#:action.attr{:action-name :move/poison
                 :actor-name  :char/hilda
                 :target-name      :char/aluxes
                 :duration    5}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/aluxes
                 :target-name      :char/hilda}
   #:action.attr{:action-name :move/fireball
                 :actor-name  :char/hilda
                 :target-name      :char/aluxes}])

(comment

  ;; just for removing warning 
  basic-attack fireball poison)