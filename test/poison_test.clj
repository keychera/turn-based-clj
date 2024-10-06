(ns poison-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [engine.timeline :refer [reduce-timeline]]
            [engine.triplestore :refer [get-attr get-entity]]
            [model.hilda :as hilda]
            [util.test-data :refer [build-history default-initial-moment]]))

(def test-poison-data
  (build-history
   [:actor/aluxes :actor/hilda]
   [hilda/poison-effect]
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

(reduce-timeline 'model.hilda default-initial-moment test-poison-data)
(deftest test-poison
  (let [actual-timeline (reduce-timeline 'model.hilda default-initial-moment test-poison-data)]
    (is (= 8 (count actual-timeline)))
    (is (= 0 (get-attr (first actual-timeline) :info/timeline :timeline/turn)))
    (is (= 1 (get-attr (nth actual-timeline 1) :info/timeline :timeline/turn)))
    (let [poison-moment (nth actual-timeline 2)]
      (is (str/includes? (get-attr poison-moment :info/moment :moment/desc) "poison"))
      (is (= :debuff/poison (get-attr poison-moment :info/moment :moment/effect-name))))
    (is (= 1 (get-attr (nth actual-timeline 3) :info/timeline :timeline/turn)))
    (is (= 2 (get-attr (nth actual-timeline 4) :info/timeline :timeline/turn)))
    (is (= 2 (get-attr (nth actual-timeline 5) :info/timeline :timeline/turn)))
    (is (= 3 (get-attr (nth actual-timeline 6) :info/timeline :timeline/turn)))
    (is (= 3 (get-attr (nth actual-timeline 7) :info/timeline :timeline/turn)))))

(def test-both-poison-data
  (build-history
   [:actor/aluxes :actor/hilda]
   [hilda/poison-effect]
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
    (let [poison-moment (nth actual-timeline 2)]
      (is (str/includes? (get-attr poison-moment :info/moment :moment/desc) "poison"))
      (is (= :debuff/poison (get-attr poison-moment :info/moment :moment/effect-name))) 
      (is (= :actor/aluxes (get-attr poison-moment :info/moment :moment/affected))))
    (is (= 1 (get-attr (nth actual-timeline 3) :info/timeline :timeline/turn)))
    (let [poison-moment (nth actual-timeline 4)]
      (is (str/includes? (get-attr poison-moment :info/moment :moment/desc) "poison"))
      (is (= :debuff/poison (get-attr poison-moment :info/moment :moment/effect-name)))
      (is (= :actor/hilda (get-attr poison-moment :info/moment :moment/affected))))
    (is (= 2 (get-attr (nth actual-timeline 5) :info/timeline :timeline/turn)))
    (is (= 2 (get-attr (nth actual-timeline 6) :info/timeline :timeline/turn)))
    (is (= 3 (get-attr (nth actual-timeline 7) :info/timeline :timeline/turn)))
    (is (= 3 (get-attr (nth actual-timeline 8) :info/timeline :timeline/turn)))))

(comment
  (reduce-timeline 'model.hilda default-initial-moment test-poison-data 2))