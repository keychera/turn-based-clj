(ns poison-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.rpl.specter :as sp]
            [engine2.timeline :as t]
            [model.topaz :as topaz]
            [pod.huahaiy.datalevin :as d]
            [util.test-data :refer [default-initial-moment with-fresh-timeline
                                    with-timeline]]))

(def test-one-poison-world
  #:world
   {:actions {:move/basic-attack 'model.topaz/basic-attack
              :move/fireball     'model.topaz/fireball
              :move/poison       'model.topaz/poison}
    :rules   [topaz/debuff-poison]
    :initial default-initial-moment})

(def history
  [#:action.attr{:action-name :move/poison
                 :actor-name  :char/hilda
                 :target-name :char/aluxes
                 :duration    1}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/aluxes
                 :target-name :char/hilda}

   #:action.attr{:action-name :move/fireball
                 :actor-name  :char/hilda
                 :target-name :char/aluxes}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/hilda
                 :target-name :char/aluxes}

   #:action.attr{:action-name :move/fireball
                 :actor-name  :char/hilda
                 :target-name :char/aluxes}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/hilda
                 :target-name :char/aluxes}])

(def one-poison-timeline
  (with-fresh-timeline
    (fn [timeline] (t/engrave! timeline test-one-poison-world history))))

(deftest count-gen-timeline
  (with-timeline one-poison-timeline
    (fn [timeline]
      (let [epoch-count (d/q '[:find (count ?epoch) . :where [_ :moment.attr/epoch ?epoch]]
                             (d/db timeline))]
        (is (= 8 epoch-count))))))

(deftest test-last-epoch
  (with-timeline one-poison-timeline
    (fn [timeline]
      (is (= 7 (t/q-last-epoch timeline))))))

(deftest test-moment-desc
  (testing "testing moment description"
    (with-timeline one-poison-timeline
      (fn [timeline]
        (is (= ":char/hilda poisons :char/aluxes ! :char/aluxes is now poisoned!"
               (->> (t/q-moment timeline 1)
                    (sp/select-one [:moment.attr/facts sp/ALL :fact/desc some?]))))))))

(deftest test-poison-interaction
  (testing "poison interactions"
    (with-timeline one-poison-timeline
      (fn [timeline]
        (let [poison-moment (t/q-moment timeline 2)]
          (is (str/includes? (sp/select-one [:moment.attr/facts sp/ALL :fact/desc some?] poison-moment)
                             "poisoned"))
          (is :event/poison-unleashed
              (sp/select-one [:moment.attr/facts sp/ALL :fact/event some?] poison-moment))
          (is :debuff/poison
              (sp/select-one [:moment.attr/facts sp/ALL :fact/effect-name some?] poison-moment)))))))

(comment
  (def test-both-poison-data
    (build-history
     [:char/aluxes :char/hilda]
     [hilda/poison-effect]
     [#:moment{:whose  :char/hilda
               :action '(-> :char/hilda (poison :char/aluxes #:effect-data{:duration 1}))}
      #:moment{:whose  :char/aluxes
               :action '(-> :char/aluxes (poison :char/hilda #:effect-data{:duration 1}))}

      #:moment{:whose  :char/hilda
               :action '(-> :char/hilda (magic-up))}
      #:moment{:whose  :char/aluxes
               :action '(-> :char/aluxes (basic-attack :char/hilda))}

      #:moment{:whose  :char/hilda
               :action '(-> :char/hilda (magic-up))}
      #:moment{:whose  :char/aluxes
               :action '(-> :char/aluxes (basic-attack :char/hilda))}]))

  (deftest test-both-poison
    (let [actual-timeline (reduce-timeline 'model.hilda default-initial-moment test-both-poison-data 3)]
      (is (= 9 (count actual-timeline)))
      (is (= 0 (get-attr (first actual-timeline) :info/timeline :timeline/turn)))
      (is (= 1 (get-attr (nth actual-timeline 1) :info/timeline :timeline/turn)))
      (let [poison-moment (nth actual-timeline 2)]
        (is (str/includes? (get-attr poison-moment :info/moment :moment/desc) "poison"))
        (is (= :debuff/poison (get-attr poison-moment :info/moment :moment/effect-name)))
        (is (= :char/aluxes (get-attr poison-moment :info/moment :moment/affected))))
      (is (= 1 (get-attr (nth actual-timeline 3) :info/timeline :timeline/turn)))
      (let [poison-moment (nth actual-timeline 4)]
        (is (str/includes? (get-attr poison-moment :info/moment :moment/desc) "poison"))
        (is (= :debuff/poison (get-attr poison-moment :info/moment :moment/effect-name)))
        (is (= :char/hilda (get-attr poison-moment :info/moment :moment/affected))))
      (is (= 2 (get-attr (nth actual-timeline 5) :info/timeline :timeline/turn)))
      (is (= 2 (get-attr (nth actual-timeline 6) :info/timeline :timeline/turn)))
      (is (= 3 (get-attr (nth actual-timeline 7) :info/timeline :timeline/turn)))
      (is (= 3 (get-attr (nth actual-timeline 8) :info/timeline :timeline/turn)))))


  (reduce-timeline 'model.hilda default-initial-moment test-poison-data 2))