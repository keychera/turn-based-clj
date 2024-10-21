(ns fua-test
  (:require [clojure.test :refer [deftest is]]
            [com.rpl.specter :as sp]
            [engine2.timeline :as t :refer [entity]]
            [model.topaz :as topaz]
            [pod.huahaiy.datalevin :as d]
            [util.test-data :refer [default-initial-moment with-fresh-timeline
                                    with-timeline]]))

(def clara-world
  #:world
   {:actions {:move/basic-attack 'model.topaz/basic-attack
              :move/fireball     'model.topaz/fireball}
    :rules   [topaz/talent-clara]
    :initial (->> default-initial-moment
                  (sp/transform [(entity :char/aluxes) :actor.attr/effects]
                                #(conj % {:effect.attr/effect-name :talent/clara})))})

(def clara-history
  [#:action.attr{:action-name :move/fireball
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
                 :target-name :char/hilda}

   #:action.attr{:action-name :move/fireball
                 :actor-name  :char/hilda
                 :target-name :char/aluxes}
   #:action.attr{:action-name :move/basic-attack
                 :actor-name  :char/aluxes
                 :target-name :char/hilda}])

(def clara-timeline
  (with-fresh-timeline
    (fn [timeline] (t/engrave! timeline clara-world clara-history))))


(deftest test-clara-talent
  (with-timeline clara-timeline
    (fn [timeline]
      (let [epoch-count (d/q '[:find (count ?epoch) . :where [_ :moment.attr/epoch ?epoch]]
                             (d/db timeline))]
        (is (= 10 epoch-count))))))
