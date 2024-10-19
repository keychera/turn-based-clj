(ns engine.timeline-test
  (:require [clojure.test :refer [deftest is]]
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
                 :actor       :char/hilda
                 :target      :char/aluxes
                 :duration    1}
   #:action.attr{:action-name :move/basic-attack
                 :actor       :char/aluxes
                 :target      :char/hilda}
   #:action.attr{:action-name :move/fireball
                 :actor       :char/hilda
                 :target      :char/aluxes}])

(deftest timeline-test
  (with-fresh-timeline
    (fn [timeline]
      (t/engrave! timeline poison-test-world history)
      (let [epoch-count (d/q '[:find (count ?epoch) .
                                   :where [_ :moment.attr/epoch ?epoch]] 
                                 (d/db timeline))]
        (is (= 5 epoch-count))))))
