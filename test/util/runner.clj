(ns util.runner
  (:require [clojure.test]
            [cognitect.test-runner :as tr]))

;; relevant https://clojurians.slack.com/archives/CLX41ASCS/p1727176496828499
(defn- do-test
  [{:keys [dirs nses patterns vars includes excludes]}]
  (let [adapted {:dir (when (seq dirs) (set dirs))
                 :namespace (when (seq nses) (set nses))
                 :namespace-regex (when (seq patterns) (map re-pattern patterns))
                 :var (when (seq vars) (set vars))
                 :include (when (seq includes) (set includes))
                 :exclude (when (seq excludes) (set excludes))}
        original-report clojure.test/report]
    (with-redefs [clojure.test/report
                  (fn [test-result]
                    (let [modified-test-result
                          (cond-> test-result
                            (and (= :error (:type test-result)) (instance? Throwable (:actual test-result)))
                            (update :actual #(-> % Throwable->map (dissoc :trace))))]
                      (original-report modified-test-result)))]
      (tr/test adapted))))


(defn run-tests [opts]
  (let [test-results (do-test opts)
        {:keys [fail error]} test-results]
    (when (pos? (+ fail error))
      (System/exit 1))))

(comment
  (run-tests {}))