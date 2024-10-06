(ns fua-test
  (:require [clojure.test :refer [deftest is]]
            [engine.timeline :refer [reduce-timeline]]
            [util.test-data :refer [default-initial-moment build-history]]))

(def clara-talent
  [[:actor/aluxes :attr/effects 1]
   [1 :effect-data/effect-name :talent/clara]])

(def clara-initial-moment
  (into default-initial-moment clara-talent))

(def blade-talent
  [[:actor/aluxes :attr/effects 2]
   [2 :effect-data/effect-name :talent/blade]
   [2 :effect-data/max-charge 2]
   [2 :effect-data/charge 0]])

(def blade-initial-moment
  (into default-initial-moment blade-talent))

(def clara-blade-initial-moment
  (-> default-initial-moment (into clara-talent) (into blade-talent)))

(def test-fua-history
  (build-history
   [:actor/aluxes :actor/hilda]
   []
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
  (let [actual-timeline (reduce-timeline 'model.hilda clara-initial-moment test-fua-history 3)]
    (is (= 10 (count actual-timeline)))))

(deftest test-blade-talent
  (let [actual-timeline (reduce-timeline 'model.hilda blade-initial-moment test-fua-history 3)]
    (is (= 8 (count actual-timeline)))))

(deftest test-clara-blade-talent
  (let [actual-timeline (reduce-timeline 'model.hilda clara-blade-initial-moment test-fua-history 3)]
    (is (= 11 (count actual-timeline)))))

(comment
  (require '[babashka.pods :as pods])
  (pods/load-pod 'huahaiy/datalevin "0.9.10")
  (require '[pod.huahaiy.datalevin :as d])

  (->> (d/q '[:find ?talent-name
              :where
              [:actor/aluxes :attr/effects ?eid]
              [?eid :effect-data/talent-name ?talent-name]]
            clara-initial-moment)))