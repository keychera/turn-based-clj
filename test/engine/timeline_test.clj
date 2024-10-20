(ns engine.timeline-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.rpl.specter :as sp]
            [engine2.timeline :as t]
            [model.topaz :as topaz]
            [pod.huahaiy.datalevin :as d]
            [util.test-data :refer [default-initial-moment with-fresh-timeline]]))

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
                 :target      :char/aluxes
                 :duration    1}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/aluxes
                 :target      :char/hilda}
   #:action.attr{:action-name :move/fireball
                 :actor-name  :char/hilda
                 :target      :char/aluxes}])

(deftest timeline-test
  (with-fresh-timeline
    (fn [timeline]
      (t/engrave! timeline poison-test-world history)
      (let [epoch-count (d/q '[:find (count ?epoch) .
                               :where [_ :moment.attr/epoch ?epoch]]
                             (d/db timeline))]
        (is (= 5 epoch-count))))))

;; specter paths test

(deftest on-effect-test
  (testing "that the correct existing effect is changed"
    (is (= (->> {:actor.attr/effects [#:effect.attr
                                       {:effect-name :debuff/poison
                                        :source-ref :char/hilda
                                        :duration 1}]}
                (sp/setval [(t/on-effect :debuff/poison :char/hilda)]
                           #:effect.attr
                            {:effect-name :debuff/poison
                             :source-ref :char/hilda
                             :duration 3}))
           {:actor.attr/effects [#:effect.attr
                                  {:effect-name :debuff/poison
                                   :source-ref :char/hilda
                                   :duration 3}]})))
  (testing "that existing effect with different args is not changed"
   (is (= (->> {:actor.attr/effects [#:effect.attr
                                     {:effect-name :debuff/poison
                                      :source-ref :char/aluxes
                                      :duration 1}]}
              (sp/setval [(t/on-effect :debuff/poison :char/hilda)]
                         #:effect.attr
                          {:effect-name :debuff/poison
                           :source-ref :char/hilda
                           :duration 3}))
         {:actor.attr/effects [#:effect.attr
                                {:effect-name :debuff/poison
                                 :source-ref :char/aluxes
                                 :duration 1}]}))))

(comment
  (with-fresh-timeline
    (fn [timeline]
      (t/engrave! timeline poison-test-world history)
      (d/q '[:find (count ?epoch) .
             :where [_ :moment.attr/epoch ?epoch]]
           (d/db timeline)))))