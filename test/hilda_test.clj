(ns hilda-test
  (:require [clojure.test :refer [deftest is testing]]
            [engine.timeline :refer [reduce-timeline]]
            [clojure.string :as str]))

(def test-initial-state
  #:state{:turn 0
          :desc "battle begins"
          :entities #:actor{:hilda #:attr{:hp 560 :mp 200}
                            :aluxes #:attr{:hp 800 :mp 10}}})

(def test-battle-data
  #:battle-data
   {:actors [:actor/aluxes :actor/hilda]
    :history-atom
    (atom [#:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (poison :actor/aluxes #:effect-data{:duration 1}))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (magic-up))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

           #:moment{:whose  :actor/hilda
                    :action '(-> :actor/hilda (magic-up))}
           #:moment{:whose  :actor/aluxes
                    :action '(-> :actor/aluxes (basic-attack :actor/hilda))}])})

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
  (reduce-timeline 'model.hilda test-initial-state test-battle-data 4))