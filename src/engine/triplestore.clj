(ns engine.triplestore
  (:require [com.rpl.specter :as sp]
            [matchete.core :as m]))

(require '[babashka.pods :as pods])
(pods/load-pod 'huahaiy/datalevin "0.9.10")
(require '[pod.huahaiy.datalevin :as d])

(defn gen-dynamic-eid [store]
  (or (some->> (d/q '[:find ?entity :where [?entity _ _]] store)
               (map first) (filter int?) sort last inc) 0))


(defn transform-entity [original-store eid transform-map]
  (->> transform-map
       (reduce (fn [store [attrkey transformation]]
                 (let [prev-attr (sp/select-one [sp/ALL #(m/match? [eid attrkey '_] %)] store)
                       add-duplicate-attr? (m/match? '[:add _] transformation)
                       transformation (if add-duplicate-attr? (second transformation) transformation)]
                   (cond
                     (= prev-attr [eid attrkey transformation]) store

                     add-duplicate-attr?
                     (do
                       (when (fn? transformation) (throw (IllegalStateException. (str "cannot transform value when adding new attribute (" attrkey ")"))))
                       (conj store [eid attrkey transformation]))

                     (nil? prev-attr)
                     (do (when (fn? transformation) (throw (IllegalStateException. (str "cannot transform value from non-exisiting attribute (" attrkey ")"))))
                         (conj store [eid attrkey transformation]))

                     :else
                     (cond
                       (nil? transformation) store
                       (fn? transformation) (sp/transform [sp/ALL #(m/match? [eid attrkey '_] %) sp/LAST] transformation store)
                       :else (sp/setval [sp/ALL #(m/match? [eid attrkey '_] %)  sp/LAST] transformation store)))))
               original-store)))

(defn remove-triples [store clause]
  (sp/setval [sp/ALL #(m/match? clause %)] sp/NONE store))

(def query d/q)

(defn query-one [query & inputs]
  (-> (apply d/q query inputs) first first))

(defn get-entity [store eid]
  (let [result (query '[:find ?attr ?attr-val :in $ ?eid
                        :where [?eid ?attr ?attr-val]] store eid)
        resolved-duplicate-attrs
        (->> result
             (group-by first)
             (map (fn [[attr values]]
                    (if (= (count values) 1)
                      [attr (->> values (map second) first)]
                      [attr (->> values (map second))]))))]
    (if (empty? result) nil (into {} resolved-duplicate-attrs))))

(defn get-attr [store eid attr]
  (query-one '[:find ?attr-val :in $ ?eid ?attr
               :where [?eid ?attr ?attr-val]]
             store eid attr))

(defn remove-attr [store eid attr]
  (sp/setval [sp/ALL #(m/match? [eid attr '_] %)] sp/NONE store))
