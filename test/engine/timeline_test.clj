(ns engine.timeline-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.rpl.specter :as sp]
            [engine2.timeline :as t]
            [model.topaz :as topaz]
            [pod.huahaiy.datalevin :as d]
            [util.test-data :refer [default-initial-moment with-fresh-timeline with-timeline]]))

(def poison-test-world
  #:world
   {:actions {:move/basic-attack 'model.topaz/basic-attack
              :move/fireball     'model.topaz/fireball
              :move/poison       'model.topaz/poison}
    :rules   [topaz/debuff-poison]
    :initial default-initial-moment})

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

(deftest timeline-test
  (with-fresh-timeline
    (fn [timeline]
      (t/engrave! timeline poison-test-world history)
      (let [epoch-count (d/q '[:find (count ?epoch) .
                               :where [_ :moment.attr/epoch ?epoch]]
                             (d/db timeline))]
        (is (= 5 epoch-count))))))

;; specter paths test

(def new-effect
  #:effect.attr
   {:effect-name :debuff/poison
    :source-name :char/hilda
    :duration 3})

(deftest on-effect-test
  (testing "that the correct existing effect is changed"
    (is (= (->> {:actor.attr/effects [#:effect.attr
                                       {:effect-name :debuff/poison
                                        :source-name :char/hilda
                                        :duration 1}]}
                (sp/setval [(t/on-effect :debuff/poison :char/hilda)] new-effect))
           {:actor.attr/effects [new-effect]})))
  (testing "that it is no-op if there is no matching effect"
    (is (= (->> {:actor.attr/effects [#:effect.attr
                                       {:effect-name :debuff/poison
                                        :source-name :char/aluxes
                                        :duration 1}]}
                (sp/setval [(t/on-effect :debuff/poison :char/hilda)] new-effect))
           {:actor.attr/effects [#:effect.attr
                                  {:effect-name :debuff/poison
                                   :source-name :char/aluxes
                                   :duration 1}]})))
  (testing "that it is no-op if there is no effect"
    (is (= (->> {:actor.attr/effects []}
                (sp/setval [(t/on-effect :debuff/poison :char/hilda)] new-effect))
           {:actor.attr/effects []}))))

(deftest add-effect-test
  (testing "that it will add new effect if there is no effect"
    (is (= (->> {:actor.attr/effects []}
                (sp/setval [t/add-effect] new-effect))
           {:actor.attr/effects [new-effect]})))
  (testing "that it will add double the effect if there are identical effects"
    (is (= (->> {:actor.attr/effects [new-effect]}
                (sp/setval [t/add-effect] new-effect))
           {:actor.attr/effects [new-effect new-effect]}))))

(deftest on-or-add-effect-test
  (testing "that it will add new effect if there is no effect"
    (is (= (->> {:actor.attr/effects []}
                (sp/setval [(t/on-or-add-effect :debuff/poison :char/hilda)] new-effect))
           {:actor.attr/effects [new-effect]})))
  (testing "that it will NOT double the effect if there are identical effects"
    (is (= (->> {:actor.attr/effects [new-effect]}
                (sp/setval [(t/on-or-add-effect :debuff/poison :char/hilda)] new-effect))
           {:actor.attr/effects [new-effect]})))
  (testing "that it update existing effect if there are identical effects"
    (is (= (->> {:actor.attr/effects [#:effect.attr
                                       {:effect-name :debuff/poison
                                        :source-name :char/hilda
                                        :duration 99}]}
                (sp/setval [(t/on-or-add-effect :debuff/poison :char/hilda)] new-effect))
           {:actor.attr/effects [new-effect]}))))

(comment
  (with-timeline "tmp/try"
    (fn [timeline]
      #_(t/engrave! timeline poison-test-world history)
      #_(d/q '[:find (count ?epoch) .
               :where [_ :moment.attr/epoch ?epoch]]
             (d/db timeline))
      (t/q-moment timeline 1))))