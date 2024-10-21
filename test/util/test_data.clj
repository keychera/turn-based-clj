(ns util.test-data
  (:require [engine.timeline :refer [timeline-schema]]
            [pod.huahaiy.datalevin :as d]))

(def default-initial-moment
  #:moment.attr
   {:epoch    0
    :entities [#:actor.attr
                {:db/id      "hilda"
                 :actor-name :char/hilda
                 :hp         560
                 :mp         200
                 :effects    []}
               #:actor.attr
                {:actor-name :char/aluxes
                 :hp         800
                 :mp         10
                 :effects    []}]})

(defn with-timeline [source do-fn]
  (let [timeline (d/get-conn source timeline-schema)]
    (try (do-fn timeline) source
         (finally (d/close timeline)))))

(defn with-fresh-timeline [do-fn]
  (with-timeline (str "tmp/test/rand-" (rand 42)) do-fn))