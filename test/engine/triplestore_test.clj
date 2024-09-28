(ns engine.triplestore-test
  (:require [clojure.test :refer [deftest is]]
            [engine.triplestore :refer [gen-dynamic-eid get-attr
                                        get-attr-multi get-entity
                                        overwrite-entity remove-attr
                                        remove-entity remove-triples
                                        transform-entity]]
            [pod.huahaiy.datalevin :as d]
            [util.test-data :refer [default-initial-moment]]))

(def store-with-no-dynamic-id
  [[:actor/hilda :attr/hp 410]
   [:actor/hilda :attr/mp 140]
   [:actor/aluxes :attr/hp 750]
   [:actor/aluxes :attr/mp 10]])

(def store
  (into store-with-no-dynamic-id
        [[:actor/aluxes :attr/effects 1]
         [:actor/aluxes :attr/effects 4]
         [1 :effect-data/effect-name :debuff/poison]
         [1 :effect-data/source :actor/hilda]
         [1 :effect-data/duration 3]]))

;; this was once our own function, but it turns out I don't have to make one
;; but still leaving this as history
(deftest test-query-one
  (is (= nil (d/q '[:find ?effect-id . :where [:actor/hilda :attr/effects ?effect-id]] store)))
  (is (= 4 (d/q '[:find ?effect-id . :where [:actor/aluxes :attr/effects ?effect-id]] store))))

(deftest test-gen-dynamic-eid
  (is (= 2 (gen-dynamic-eid store)))
  (is (= 0 (gen-dynamic-eid store-with-no-dynamic-id))))

(deftest test-get-attr
  (is (= 750 (get-attr store :actor/aluxes :attr/hp)))
  (is (= nil (get-attr store :actor/hilda :attr/effects)))
  (is (= nil (get-attr store :actor/topaz :attr/hp))))

(deftest test-get-attr-multi
  (is (= [:actor/hilda :actor/aluxes]
         (get-attr-multi default-initial-moment :info/moment :moment/actors))))

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
          [:actor/aluxes :attr/effects 1]
          [:actor/aluxes :attr/effects 4]]
         (remove-triples store '[1 _ _]))))

(deftest test-get-entity
  (is (= #:attr{:hp 750 :mp 10 :effects '(1 4)}
         (get-entity store :actor/aluxes)))
  (is (= #:effect-data{:effect-name :debuff/poison :source :actor/hilda :duration 3}
         (get-entity store 1)))
  (is (= nil (get-entity store :actor/topaz))))

(deftest test-remove-entity
  (is (= [[:actor/hilda :attr/hp 410]
          [:actor/hilda :attr/mp 140]
          [1 :effect-data/effect-name :debuff/poison]
          [1 :effect-data/source :actor/hilda]
          [1 :effect-data/duration 3]]
         (remove-entity store :actor/aluxes)))
  (is (= [[:actor/aluxes :attr/hp 750]
          [:actor/aluxes :attr/mp 10]]
         (remove-entity store-with-no-dynamic-id :actor/hilda)))
  (is (= [[:actor/hilda :attr/hp 410]
          [:actor/hilda :attr/mp 140]
          [:actor/aluxes :attr/hp 750]
          [:actor/aluxes :attr/mp 10]
          [:actor/aluxes :attr/effects 1]
          [:actor/aluxes :attr/effects 4]]
         (remove-entity store 1)))
  ;; do we need transitive remove? let's just kiss for now
  (is (= store-with-no-dynamic-id
         (remove-entity store-with-no-dynamic-id :actor/topaz))))

(deftest test-overwrite-entity
  (is (= [[:actor/aluxes :attr/hp 750]
          [:actor/aluxes :attr/mp 10]
          [:actor/hilda :attr/hp -99]
          [:actor/hilda :attr/effects 99]]
         (overwrite-entity store-with-no-dynamic-id :actor/hilda
                           {:attr/hp -99 :attr/effects 99}))))

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
          [:actor/aluxes :attr/effects 99]]
         (transform-entity simple-store :actor/aluxes {:attr/effects 99})))
  (is (= [[:actor/aluxes :attr/hp 1]
          [:actor/aluxes :attr/hp 99]]
         (transform-entity simple-store :actor/aluxes {:attr/hp [:add 99]})))
  (is (= [[:actor/aluxes :attr/hp 99]]
         (transform-entity [] :actor/aluxes {:attr/hp [:add 99]})))

  ;; current behaviour is that multiple attributes can't have the same value
  ;; all facts in the store are distinct
  (is (= simple-store
         (transform-entity simple-store :actor/aluxes {:attr/hp [:add 1]})))
  (is (= (into simple-store
               [[:actor/aluxes :attr/hp 99]
                [:actor/aluxes :attr/hp :debuff/poison]
                [:actor/aluxes :attr/hp "topaz"]])
         (transform-entity simple-store :actor/aluxes {:attr/hp [:add 1 99 :debuff/poison "topaz"]})))

  (is (thrown? IllegalStateException
               (transform-entity simple-store :actor/aluxes {:attr/effects [:add inc]})))
  (is (thrown? IllegalStateException
               (transform-entity simple-store :actor/aluxes {:attr/effects [:add 2 3 inc 4]})))
  (is (thrown? IllegalStateException
               (transform-entity simple-store :actor/aluxes {:attr/effects inc}))))