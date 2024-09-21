(ns hilda-test
  (:require [clojure.test :refer [deftest is testing]]
            [engine.timeline :refer [reduce-timeline]]))

(def test-initial-state
  #:state{:moment 0
          :desc "battle begins"
          :entities #:actor{:hilda #:attr{:hp 560 :mp 200}
                            :aluxes #:attr{:hp 800 :mp 10}}})

(def test-history
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
                  :action '(-> :actor/aluxes (basic-attack :actor/hilda))}]))


(deftest test-poison
  (testing "Test poison interaction"
    (let [reduced-timeline (reduce-timeline 'model.hilda test-initial-state @test-history)] 
      (is (= 7 (count reduced-timeline))))))

(comment
  (reduce-timeline 'model.hilda test-initial-state @test-history))