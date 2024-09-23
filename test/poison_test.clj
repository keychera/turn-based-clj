(ns poison-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [engine.timeline :refer [reduce-timeline]]
            [engine.triplestore :refer [query-one]]
            [util.test-data :refer [build-history default-initial-state]]))

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

(defn query-info [attr-key store]
  (query-one '[:find ?turn :in $ ?attr-key :where [:info/state ?attr-key ?turn]] store attr-key))

(deftest test-poison
  (testing "Test poison interaction"
    (let [actual-timeline (reduce-timeline 'model.hilda default-initial-state test-poison-data)]
      (is (= 8 (count actual-timeline)))
      (is (= 0 (query-info :state/turn (first actual-timeline))))
      (is (= 1 (query-info :state/turn (nth actual-timeline 1))))
      (is (= 1 (query-info :state/turn (nth actual-timeline 2))))
      (let [poison-moment (nth actual-timeline 3)]
        (is (str/includes? (query-info :state/desc poison-moment) "poison"))
        (is (= 2 (query-info :state/turn poison-moment))))
      (is (= 2 (query-info :state/turn (nth actual-timeline 4))))
      (is (= 2 (query-info :state/turn (nth actual-timeline 5))))
      (is (= 3 (query-info :state/turn (nth actual-timeline 6))))
      (is (= 3 (query-info :state/turn (nth actual-timeline 7)))))))

(comment
  (reduce-timeline 'model.hilda default-initial-state test-poison-data 2))