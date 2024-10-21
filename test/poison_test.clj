(ns poison-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [com.rpl.specter :as sp]
            [engine2.timeline :as t]
            [model.topaz :as topaz]
            [pod.huahaiy.datalevin :as d]
            [util.test-data :refer [default-initial-moment with-fresh-timeline
                                    with-timeline]]))

(def poison-world
  #:world
   {:actions {:move/basic-attack 'model.topaz/basic-attack
              :move/fireball     'model.topaz/fireball
              :move/poison       'model.topaz/poison}
    :rules   [topaz/debuff-poison topaz/remove-effect-on-duration-0]
    :initial default-initial-moment})

(def one-poison-history
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
                 :actor-name  :char/aluxes
                 :target-name :char/hilda}
   
   #:action.attr{:action-name :move/fireball
                 :actor-name  :char/hilda
                 :target-name :char/aluxes}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/aluxes
                 :target-name :char/hilda}])

(def one-poison-timeline
  (with-fresh-timeline
    (fn [timeline] (t/engrave! timeline poison-world one-poison-history))))

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

(def both-poison-history
  [#:action.attr{:action-name :move/poison
                 :actor-name  :char/hilda
                 :target-name :char/aluxes
                 :duration    2}
   #:action.attr{:action-name :move/poison
                 :actor-name  :char/aluxes
                 :target-name :char/hilda
                 :duration    1}

   #:action.attr{:action-name :move/fireball
                 :actor-name  :char/hilda
                 :target-name :char/aluxes}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/aluxes
                 :target-name :char/hilda}

   #:action.attr{:action-name :move/fireball
                 :actor-name  :char/hilda
                 :target-name :char/aluxes}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/aluxes
                 :target-name :char/hilda}])

(def both-poison-timeline
  (with-fresh-timeline
    (fn [timeline] (t/engrave! timeline poison-world both-poison-history))))

(deftest count-gen-both-poison-timeline
  (with-timeline both-poison-timeline
    (fn [timeline]
      (let [epoch-count (d/q '[:find (count ?epoch) . :where [_ :moment.attr/epoch ?epoch]]
                             (d/db timeline))]
        (is (= 10 epoch-count))))))

(deftest test-last-epoch-both-poison-timeline
  (with-timeline both-poison-timeline
    (fn [timeline]
      (is (= 9 (t/q-last-epoch timeline))))))

(deftest test-both-poison-interaction
  (testing "poison interactions"
    (with-timeline both-poison-timeline
      (fn [timeline]
        (let [poison-moment (t/q-moment timeline 2)]
          (is (str/includes? (sp/select-one [:moment.attr/facts sp/ALL :fact/desc some?] poison-moment)
                             "poisoned"))
          (is (= :event/poison-unleashed
                 (sp/select-one [:moment.attr/facts sp/ALL :fact/event some?] poison-moment)))
          (is (= :debuff/poison
                 (sp/select-one [:moment.attr/facts sp/ALL :fact/effect-name some?] poison-moment)))
          (is (= :char/hilda
                 (sp/select-one [:moment.attr/facts sp/ALL :fact/source-name some?] poison-moment))))
        (let [poison-moment (t/q-moment timeline 4)]
          (is (str/includes? (sp/select-one [:moment.attr/facts sp/ALL :fact/desc some?] poison-moment)
                             "poisoned"))
          (is (= :event/poison-unleashed
                 (sp/select-one [:moment.attr/facts sp/ALL :fact/event some?] poison-moment)))
          (is (= :debuff/poison
                 (sp/select-one [:moment.attr/facts sp/ALL :fact/effect-name some?] poison-moment)))
          (is (= :char/aluxes
                 (sp/select-one [:moment.attr/facts sp/ALL :fact/source-name some?] poison-moment))))
        (let [poison-moment (t/q-moment timeline 6)]
          (is (str/includes? (sp/select-one [:moment.attr/facts sp/ALL :fact/desc some?] poison-moment)
                             "poisoned"))
          (is (= :event/poison-unleashed
                 (sp/select-one [:moment.attr/facts sp/ALL :fact/event some?] poison-moment)))
          (is (= :debuff/poison
                 (sp/select-one [:moment.attr/facts sp/ALL :fact/effect-name some?] poison-moment)))
          (is (= :char/aluxes
                 (sp/select-one [:moment.attr/facts sp/ALL :fact/source-name some?] poison-moment))))
        (let [poison-moment (t/q-moment timeline 8)]
          (is (= :move/basic-attack
                 (sp/select-one [:moment.attr/facts sp/ALL :action.attr/action-name some?] poison-moment))))))))
