(ns engine.timeline-test
  (:require [clojure.test :refer [deftest is]]
            [engine.timeline :refer [reduce-timeline]]
            [util.test-data :refer [build-history default-initial-moment]]))

(def timeline-test-data
  (build-history
   [:actor/aluxes :actor/hilda]
   [#:moment{:whose  :actor/hilda
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

(deftest timeline-limit-0-test
  (let [actual-timeline (reduce-timeline 'model.hilda default-initial-moment timeline-test-data 0)]
    (is (= 1 (count actual-timeline)))))

(deftest timeline-limit-1-test
  (let [actual-timeline (reduce-timeline 'model.hilda default-initial-moment timeline-test-data 1)]
    (is (= 3 (count actual-timeline)))))

(deftest timeline-no-limit-test
  (let [actual-timeline (reduce-timeline 'model.hilda default-initial-moment timeline-test-data)]
    (is (= 8 (count actual-timeline))))) 