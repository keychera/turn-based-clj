(ns fua-test
  (:require [clojure.test :refer [deftest is]]
            [engine.timeline :refer [reduce-timeline]]
            [util.test-data :refer [default-initial-state build-history]]))

(def fua-default
  (into default-initial-state
        [[:actor/aluxes :attr/effect 0]
         [0 :effect-data/effect-name :debuff/poison]
         [0 :effect-data/duration 3]]))

(def clara-talent
  [[:actor/aluxes :attr/effect 1]
   [1 :effect-data/effect-name :talent/clara]])

(def clara-initial-state
  (into fua-default clara-talent))

(def blade-talent
  [[:actor/aluxes :attr/effect 2]
   [2 :effect-data/effect-name :talent/blade]
   [2 :effect-data/max-charge 2]
   [2 :effect-data/charge 0]])

(def blade-initial-state
  (into fua-default blade-talent))

(def clara-blade-initial-state
  (-> fua-default (into clara-talent) (into blade-talent)))

(def test-fua-history
  (build-history
   [:actor/aluxes :actor/hilda]
   [#:moment{:whose  :actor/hilda
             :action '(-> :actor/hilda (fireball :actor/aluxes))}
    #:moment{:whose  :actor/aluxes
             :action '(-> :actor/aluxes (basic-attack :actor/hilda))}
    #:moment{:whose  :actor/hilda
             :action '(-> :actor/hilda (fireball :actor/aluxes))}
    #:moment{:whose  :actor/aluxes
             :action '(-> :actor/aluxes (basic-attack :actor/hilda))}
    #:moment{:whose  :actor/hilda
             :action '(-> :actor/hilda (fireball :actor/aluxes))}
    #:moment{:whose  :actor/aluxes
             :action '(-> :actor/aluxes (basic-attack :actor/hilda))}]))

(deftest test-clara-talent
  (let [actual-timeline (reduce-timeline 'model.hilda clara-initial-state test-fua-history)]
    (is (= 9 (count actual-timeline)))))

(deftest test-blade-talent
  (let [actual-timeline (reduce-timeline 'model.hilda blade-initial-state test-fua-history)]
    (is (= 7 (count actual-timeline)))))

(deftest test-clara-blade-talent
  (let [actual-timeline (reduce-timeline 'model.hilda clara-blade-initial-state test-fua-history)]
    (is (= 10 (count actual-timeline)))))

(comment
  (require '[babashka.pods :as pods])
  (pods/load-pod 'huahaiy/datalevin "0.9.10")
  (require '[pod.huahaiy.datalevin :as d])

  (->> (d/q '[:find ?talent-name
              :where
              [:actor/aluxes :attr/effect ?eid]
              [?eid :effect-data/talent-name ?talent-name]]
            clara-initial-state)))