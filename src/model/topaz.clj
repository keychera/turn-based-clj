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
                                                       :effect-name   :debuff/poison
                                                       :damage        damage
                                                       :source-name   ?source-name
                                                       :affected-name affected-name})))))})

(def talent-clara
  #:rule
   {:rule-name       :talent/clara
    :rules-to-inject '[[(talent-clara? ?s.current-moment ?name-with-talent)
                        [?s.current-moment :moment.attr/entities ?id-with-talent]
                        [?id-with-talent :actor.attr/effects ?eff-id]
                        [?id-with-talent :actor.attr/actor-name ?name-with-talent]
                        [?eff-id :effect.attr/effect-name :talent/clara]]
                       [(targetted? ?s.current-moment ?attacker-name ?targetted-name ?action-name)
                        [?s.current-moment :moment.attr/facts ?fact-id]
                        [?s.current-moment :moment.attr/entities ?attacker-id]
                        [?attacker-id :actor.attr/actor-name ?attacker-name]
                        [?fact-id :action.attr/actor-name ?attacker-name]
                        [?fact-id :action.attr/action-name ?action-name]
                        [?fact-id :action.attr/target-name ?targetted-name]]]
    :activation      '[:find [?targetted-name ?attacker-name]
                       :where
                       [(= ?s.timing :timing/after-action)]
                       (talent-clara? ?s.current-moment ?targetted-name)
                       (targetted? ?s.current-moment ?attacker-name ?targetted-name ?action-name)
                       [(= ?action-name :move/fireball)]
                       [(= ?s.who-acts ?attacker-name)]]
    :rule-fn         (fn [moment [?targetted-name ?attacker-name]]
                       (let [targeted-entity (sp/select-one [(entity ?targetted-name)] moment)
                             attacker-hp     (:actor.attr/hp targeted-entity)
                             damage          (Math/floor (/ attacker-hp 10))]
                         (->> moment
                              (sp/transform [(entity ?attacker-name)]
                                            #(update % :actor.attr/hp - damage))
                              (sp/transform [:moment.attr/facts]
                                            #(conj % #:fact{:desc          (str ?targetted-name " countered " ?attacker-name "'s attack with " damage " damage!")
                                                            :event         :event/clara-talent-unleashed
                                                            :damage        damage
                                                            :affected-name ?attacker-name
                                                            :source-name   ?targetted-name})))))})

(def remove-effect-on-duration-0
  #:rule
   {:rule-name  :effect/remove-0-duration
    :silent?    true
    :activation '[:find ?affected-name ?effect-name ?source-name ?epoch
                  :where
                  [?s.current-moment :moment.attr/epoch ?epoch]
                  [?s.current-moment :moment.attr/entities ?affected-id]
                  [?affected-id :actor.attr/actor-name ?affected-name]
                  [?affected-id :actor.attr/effects ?eff-id]
                  [?eff-id :effect.attr/effect-name ?effect-name]
                  [?eff-id :effect.attr/source-name ?source-name]
                  [?eff-id :effect.attr/duration 0]]
    :rule-fn    (fn [moment effects-to-rmv]
                  (loop [new-moment              moment
                         [to-remove & remaining] effects-to-rmv]
                    (let [?affected-name (get to-remove 0)
                          ?effect-name   (get to-remove 1)
                          ?source-name   (get to-remove 2)
                          new-moment     (->> new-moment
                                              (sp/setval [(entity ?affected-name) (on-effect ?effect-name ?source-name)] sp/NONE)
                                              (sp/transform [:moment.attr/facts]
                                                            #(conj % #:fact{:desc        (str ?effect-name " removed!")
                                                                            :event       :event/effect-removed
                                                                            :effect-name ?effect-name})))]
                      (if (empty? remaining) new-moment
                          (recur new-moment remaining)))))})

(def world
  #:world
   {:actions {:move/basic-attack 'model.topaz/basic-attack
              :move/fireball     'model.topaz/fireball
              :move/poison       'model.topaz/poison}
    :rules   [debuff-poison talent-clara
              remove-effect-on-duration-0]
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
                 :target-name :char/aluxes
                 :duration    1}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/aluxes
                 :target-name :char/hilda}
   #:action.attr{:action-name :move/fireball
                 :actor-name  :char/hilda
                 :target-name :char/aluxes}])

(comment

  ;; just for removing warning 
  basic-attack fireball poison)