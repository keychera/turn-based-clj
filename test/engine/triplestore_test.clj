(ns engine.triplestore-test
  (:require [clojure.test :refer [deftest is]]
            [engine.triplestore :refer [gen-dynamic-eid get-attr get-entity
                                        query-one remove-attr remove-triples
                                        transform-entity]]))

(def store-with-no-dynamic-id
  [[:actor/hilda :attr/hp 410]
   [:actor/hilda :attr/mp 140]
   [:actor/aluxes :attr/hp 750]
   [:actor/aluxes :attr/mp 10]])

(def store
  (into store-with-no-dynamic-id
        [[:actor/aluxes :attr/effect 1]
         [:actor/aluxes :attr/effect 4]
         [1 :effect-data/effect-name :debuff/poison]
         [1 :effect-data/source :actor/hilda]
         [1 :effect-data/duration 3]]))


(deftest test-query-one
  (is (= nil (query-one '[:find ?effect-id :where [:actor/hilda :attr/effect ?effect-id]] store)))
  (is (= 4 (query-one '[:find ?effect-id :where [:actor/aluxes :attr/effect ?effect-id]] store))))

(deftest test-get-entity
  (is (= #:attr{:hp 750 :mp 10 :effect '(4 1)}
         (get-entity store :actor/aluxes)))
  (is (= #:effect-data{:effect-name :debuff/poison :source :actor/hilda :duration 3}
         (get-entity store 1)))
  (is (= nil (get-entity store :actor/topaz))))

(deftest test-get-attr
  (is (= 750 (get-attr store :actor/aluxes :attr/hp)))
  (is (= nil (get-attr store :actor/hilda :attr/effect)))
  (is (= nil (get-attr store :actor/topaz :attr/hp))))

(deftest test-remove-attr
  (is (= [[:actor/hilda :attr/hp 410]
          [:actor/hilda :attr/mp 140] 
          [:actor/aluxes :attr/mp 10]]
         (remove-attr store-with-no-dynamic-id :actor/aluxes :attr/hp))))

(deftest test-remove-triples
  (is (= [[:actor/hilda :attr/hp 410]
          [:actor/hilda :attr/mp 140]
          [:actor/aluxes :attr/hp 750]
          [:actor/aluxes :attr/mp 10]
          [:actor/aluxes :attr/effect 1]
          [:actor/aluxes :attr/effect 4]]
         (remove-triples store '[1 _ _]))))

(deftest test-gen-dynamic-eid
  (is (= 2 (gen-dynamic-eid store)))
  (is (= 0 (gen-dynamic-eid store-with-no-dynamic-id))))

(def simple-store
  [[:actor/aluxes :attr/hp 1]])

(deftest test-transform-entity
  (is (= [[:actor/aluxes :attr/hp 99]]
         (transform-entity simple-store :actor/aluxes {:attr/hp 99})))
  (is (= [[:actor/aluxes :attr/hp 2]]
         (transform-entity simple-store :actor/aluxes {:attr/hp inc})))
  (is (= simple-store
         (transform-entity simple-store :actor/aluxes {:attr/hp nil})))
  (is (= [[:actor/aluxes :attr/hp 1]
          [:actor/aluxes :attr/effect 99]]
         (transform-entity simple-store :actor/aluxes {:attr/effect 99})))
  (is (= [[:actor/aluxes :attr/hp 1]
          [:actor/aluxes :attr/hp 99]]
         (transform-entity simple-store :actor/aluxes {:attr/hp [:add 99]})))
  (is (= [[:actor/aluxes :attr/hp 99]]
         (transform-entity [] :actor/aluxes {:attr/hp [:add 99]})))
  (is (= simple-store
         (transform-entity simple-store :actor/aluxes {:attr/hp [:add 1]})))
  (is (thrown? IllegalStateException
               (transform-entity simple-store :actor/aluxes {:attr/effect [:add inc]})))
  (is (thrown? IllegalStateException
               (transform-entity simple-store :actor/aluxes {:attr/effect inc}))))