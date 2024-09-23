(ns engine.triplestore
  (:require [com.rpl.specter :as sp]
            [matchete.core :as m]
            [babashka.pods :as pods]))

(pods/load-pod 'huahaiy/datalevin "0.9.10")
(require '[pod.huahaiy.datalevin :as d])

(defn gen-dynamic-eid [store]
  (or (some->> (d/q '[:find ?entity :where [?entity _ _]] store)
               (map first) (filter int?) sort last inc) 0))

(defn transform-entity [original-store eid transform-map]
  (->> transform-map
       (reduce (fn [store [attrkey transformation]]
                 (let [prev-attr (sp/select-one [sp/ALL #(m/match? [eid attrkey '_] %)] store)]
                   (if (nil? prev-attr)
                     (do
                       (when (fn? transformation) (throw (IllegalStateException. (str "cannot transform value from non-exisiting attribute (" attrkey ")"))))
                       (conj store [eid attrkey transformation]))
                     (cond
                       (nil? transformation) (sp/setval [sp/ALL #(m/match? [eid attrkey '_] %)] sp/NONE store)
                       (fn? transformation) (sp/transform [sp/ALL #(m/match? [eid attrkey '_] %) sp/LAST] transformation store)
                       :else (sp/setval [sp/ALL #(m/match? [eid attrkey '_] %)  sp/LAST] transformation store)))))
               original-store)))

(defn remove-triples [store clause]
  (sp/setval [sp/ALL #(m/match? clause %)] sp/NONE store))

(defn get-entity [store eid]
  (->> (d/q '[:find ?effect-data ?data :in $ ?eid
              :where [?eid ?effect-data ?data]]
            store eid)
       (into {})))

(def query d/q)

(defn query-one [query & inputs]
  (-> (apply d/q query inputs) first first))