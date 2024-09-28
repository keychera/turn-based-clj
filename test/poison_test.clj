(ns poison-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [engine.timeline :refer [reduce-timeline]]
            [engine.triplestore :refer [get-attr]]
            [util.test-data :refer [build-history default-initial-moment]]))

(def test-poison-poison
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

(deftest test-poison
  (testing "Test poison interaction"
    (let [actual-timeline (reduce-timeline 'model.hilda default-initial-moment test-poison-poison)]
      (is (= 8 (count actual-timeline)))
      (is (= 0 (get-attr (first actual-timeline) :info/moment :moment/turn)))
      (is (= 1 (get-attr (nth actual-timeline 1) :info/moment :moment/turn)))
      (is (= 1 (get-attr (nth actual-timeline 2) :info/moment :moment/turn)))
      (let [poison-moment (nth actual-timeline 3)]
        (is (str/includes? (get-attr poison-moment :info/moment :moment/desc) "poison"))
        (is (= (get-attr poison-moment :info/moment :effect-data/effect-name) :debuff/poison)))
      (is (= 2 (get-attr (nth actual-timeline 4) :info/moment :moment/turn)))
      (is (= 2 (get-attr (nth actual-timeline 5) :info/moment :moment/turn)))
      (is (= 3 (get-attr (nth actual-timeline 6) :info/moment :moment/turn)))
      (is (= 3 (get-attr (nth actual-timeline 7) :info/moment :moment/turn))))))

(comment
  (reduce-timeline 'model.hilda default-initial-moment test-poison-poison 2))