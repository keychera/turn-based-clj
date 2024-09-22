(ns util.runner
  (:require [cognitect.test-runner :as tr]))

(defn- do-test
  [{:keys [dirs nses patterns vars includes excludes]}]
  (let [adapted {:dir (when (seq dirs) (set dirs))
                 :namespace (when (seq nses) (set nses))
                 :namespace-regex (when (seq patterns) (map re-pattern patterns))
                 :var (when (seq vars) (set vars))
                 :include (when (seq includes) (set includes))
                 :exclude (when (seq excludes) (set excludes))}]
    (tr/test adapted)))

(defn run-tests [opts]
  (let [test-results (do-test opts)
        {:keys [fail error]} test-results]
    (when (pos? (+ fail error))
      (System/exit 1))))

(comment
  (run-tests {}))