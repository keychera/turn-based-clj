(ns hilda-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [engine.timeline :refer [reduce-timeline]]
            [test-data :refer [test-battle-data test-initial-state]]))

(deftest test-poison
  (testing "Test poison interaction"
    (let [actual-timeline (reduce-timeline 'model.hilda test-initial-state test-battle-data)]
      (is (= 8 (count actual-timeline)))
      (is (= 0 (:state/turn (first actual-timeline))))
      (is (= 1 (:state/turn (nth actual-timeline 1))))
      (is (= 1 (:state/turn (nth actual-timeline 2))))
      (let [poison-moment (nth actual-timeline 3)]
        (is (str/includes? (:state/desc poison-moment) "poison"))
        (is (= 2 (:state/turn poison-moment))))
      (is (= 2 (:state/turn (nth actual-timeline 4))))
      (is (= 2 (:state/turn (nth actual-timeline 5))))
      (is (= 3 (:state/turn (nth actual-timeline 6))))
      (is (= 3 (:state/turn (nth actual-timeline 7)))))))

(comment
  (reduce-timeline 'model.hilda test-initial-state test-battle-data 1))