(ns model.topaz
  (:require [com.rpl.specter :as sp]
            [engine2.timeline :refer [effect-on entity]]))

(defn basic-attack [actor target]
  (fn [moment]
    (let [damage 50]
      (->> moment
           (sp/setval    [:moment.attr/desc] (str actor " attacks " target " for " damage " damage!"))
           (sp/transform [(entity target)]
                         #(update % :actor.attr/hp - damage))))))

(defn fireball [actor target]
  (fn [moment]
    (let [mp-cost 15 damage 50]
      (->> moment
           (sp/setval    [:moment.attr/desc] (str actor " cast fireball towards " target " for " damage " damage!"))
           (sp/transform [(entity target)]
                         (comp #(update % :actor.attr/mp - mp-cost)
                               #(update % :actor.attr/hp - damage)))))))

(defn poison
  ([actor target] (poison actor target #:effect.attr{:duration 3}))
  ([actor target {:effect.attr/keys [duration]}]
   (fn [moment]
     (let [mp-cost 30 effect-name :debuff/poison
           actor-entity (sp/select-one [(entity :char/hilda)] moment)]
       (->> moment
            (sp/setval    [:moment.attr/desc] (str actor " poisons " target " ! " target " is now poisoned!"))
            (sp/transform [(entity actor)]
                          #(update % :actor.attr/mp - mp-cost))
            (sp/setval    [(effect-on target :debuff/poison)]
                          #:effect.attr{:effect-name effect-name
                                        :source (:db/id actor-entity)
                                        :duration duration}))))))

(def initial-moment
  #:moment.attr
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
                             :duration    1}]}]})

(def history
  [#:action.attr
    {:actor  :char/aluxes
     :act-expr '(poison :char/hilda :char/aluxes)}
   #:action.attr
    {:actor  :char/aluxes
     :act-expr '(basic-attack :char/aluxes :char/hilda)}
   #:action.attr
    {:actor  :char/aluxes
     :act-expr '(fireball :char/hilda :char/aluxes)}])

(comment

  ;; just for removing warning 
  basic-attack fireball poison)