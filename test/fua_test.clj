(ns fua-test
  (:require [clojure.test :refer [deftest is testing]]
            [engine.timeline :refer [reduce-timeline]]
            [util.test-data :refer [build-history default-initial-state]]))

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

(deftest test-fua
  (testing "Test poison interaction"
    (let [actual-timeline (reduce-timeline 'model.hilda default-initial-state test-fua-data)]
      (is (= 7 (count actual-timeline))))))