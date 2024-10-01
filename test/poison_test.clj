(ns poison-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [engine.timeline :refer [reduce-timeline]]
            [engine.triplestore :refer [get-attr get-entity]]
            [util.test-data :refer [build-history default-initial-moment]]))

(def test-poison-data
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

(deftest test-info-moment
  (let [actual-timeline (reduce-timeline 'model.hilda default-initial-moment test-poison-data 1)]
    (is (= #:moment{:desc ":actor/hilda poisons :actor/aluxes ! :actor/aluxes is now poisoned!"
                    :whose  :actor/hilda
                    :action '(-> :actor/hilda (poison :actor/aluxes #:effect-data{:duration 1}))}
           (get-entity (nth actual-timeline 1) :info/moment)))))

(deftest test-poison
  (let [actual-timeline (reduce-timeline 'model.hilda default-initial-moment test-poison-data)]
    (is (= 8 (count actual-timeline)))
    (is (= 0 (get-attr (first actual-timeline) :info/timeline :timeline/turn)))
    (is (= 1 (get-attr (nth actual-timeline 1) :info/timeline :timeline/turn)))
    (is (= 1 (get-attr (nth actual-timeline 2) :info/timeline :timeline/turn)))
    (let [poison-moment (nth actual-timeline 3)]
      (is (str/includes? (get-attr poison-moment :info/moment :moment/desc) "poison"))
      (is (= (get-attr poison-moment :info/moment :moment/effect-name) :debuff/poison)))
    (is (= 2 (get-attr (nth actual-timeline 4) :info/timeline :timeline/turn)))
    (is (= 2 (get-attr (nth actual-timeline 5) :info/timeline :timeline/turn)))
    (is (= 3 (get-attr (nth actual-timeline 6) :info/timeline :timeline/turn)))
    (is (= 3 (get-attr (nth actual-timeline 7) :info/timeline :timeline/turn)))))

(def test-both-poison-data
  (build-history
   [:actor/aluxes :actor/hilda]
   [#:moment{:whose  :actor/hilda
             :action '(-> :actor/hilda (poison :actor/aluxes #:effect-data{:duration 1}))}
    #:moment{:whose  :actor/aluxes
             :action '(-> :actor/aluxes (poison :actor/hilda #:effect-data{:duration 1}))}

    #:moment{:whose  :actor/hilda
             :action '(-> :actor/hilda (magic-up))}
    #:moment{:whose  :actor/aluxes
             :action '(-> :actor/aluxes (basic-attack :actor/hilda))}

    #:moment{:whose  :actor/hilda
             :action '(-> :actor/hilda (magic-up))}
    #:moment{:whose  :actor/aluxes
             :action '(-> :actor/aluxes (basic-attack :actor/hilda))}]))

(deftest test-both-poison
  (let [actual-timeline (reduce-timeline 'model.hilda default-initial-moment test-both-poison-data)]
    (is (= 9 (count actual-timeline)))
    (is (= 0 (get-attr (first actual-timeline) :info/timeline :timeline/turn)))
    (is (= 1 (get-attr (nth actual-timeline 1) :info/timeline :timeline/turn)))
    (is (= 1 (get-attr (nth actual-timeline 2) :info/timeline :timeline/turn)))
    (let [poison-moment (nth actual-timeline 3)]
      (is (str/includes? (get-attr poison-moment :info/moment :moment/desc) "poison"))
      (is (= (get-attr poison-moment :info/moment :moment/effect-name) :debuff/poison))
      ;; the order the poison unleashed needs some rethinking
      (is (= (get-attr poison-moment :info/moment :moment/affected) :actor/hilda)))
    (let [poison-moment (nth actual-timeline 4)]
      (is (str/includes? (get-attr poison-moment :info/moment :moment/desc) "poison"))
      (is (= (get-attr poison-moment :info/moment :moment/effect-name) :debuff/poison))
      (is (= (get-attr poison-moment :info/moment :moment/affected) :actor/aluxes)))
    (is (= 2 (get-attr (nth actual-timeline 5) :info/timeline :timeline/turn)))
    (is (= 2 (get-attr (nth actual-timeline 6) :info/timeline :timeline/turn)))
    (is (= 3 (get-attr (nth actual-timeline 7) :info/timeline :timeline/turn)))
    (is (= 3 (get-attr (nth actual-timeline 8) :info/timeline :timeline/turn)))))

(comment
  (reduce-timeline 'model.hilda default-initial-moment test-poison-data 2))