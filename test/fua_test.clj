(ns fua-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [engine.timeline :refer [reduce-timeline]]
            [util.test-data :refer [build-history]]))

(def fua-initial-state
  #:state{:turn 0
          :desc "battle begins"
          :entities #:actor{:hilda #:attr{:hp 560 :mp 200}
                            :aluxes #:attr{:hp 800 :mp 10 :effect
                                           {:talent/blade #:effect-data{:counter 5 :trigger :event/on-state-change}}}}})

(def test-fua-data
  (build-history
   [:actor/aluxes :actor/hilda]
   [#:moment{:whose  :actor/hilda
             :action '(-> :actor/hilda (fireball :actor/aluxes))}
    #:moment{:whose  :actor/aluxes
             :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

    #:moment{:whose  :actor/hilda
             :action '(-> :actor/hilda (magic-up))}
    #:moment{:whose  :actor/aluxes
             :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

    #:moment{:whose  :actor/hilda
             :action '(-> :actor/hilda (magic-up))}
    #:moment{:whose  :actor/aluxes
             :action '(-> :actor/aluxes (basic-attack :actor/hilda))}]))

(deftest test-poison
  (testing "Test poison interaction"
    (let [actual-timeline (reduce-timeline 'model.hilda fua-initial-state test-fua-data)]
      (is (= 8 (count actual-timeline)))
      (is (= 0 (:state/turn (first actual-timeline))))
      (is (= 1 (:state/turn (nth actual-timeline 1))))
      (is (= 1 (:state/turn (nth actual-timeline 2))))
      (let [poison-moment (nth actual-timeline 3)]
        (is (str/includes? (:state/desc poison-moment) "poison"))
        (is (= 2 (:state/turn poison-moment))))
      (is (= 2 (:state/turn (nth actual-timeline 4))))
      (is (= 2 (:state/turn (nth actual-timeline 5))))
      (is (= 3 (:state/turn (nth actual-timeline 6)))))))