(ns timeline-test
  (:require [clojure.test :refer [deftest is]]
            [engine.timeline :refer [reduce-timeline]]
            [test-data :refer [test-battle-data test-initial-state]]))

(deftest timeline-limit-0-test
  (let [actual-timeline (reduce-timeline 'model.hilda test-initial-state test-battle-data 0)]
    (is (= 1 (count actual-timeline)))))

(deftest timeline-limit-1-test
  (let [actual-timeline (reduce-timeline 'model.hilda test-initial-state test-battle-data 1)]
    (is (= 3 (count actual-timeline)))))

(deftest timeline-no-limit-test
  (let [actual-timeline (reduce-timeline 'model.hilda test-initial-state test-battle-data)]
    (is (= 8 (count actual-timeline))))) 