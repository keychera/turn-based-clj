(ns engine.triplestore
  (:require [com.rpl.specter :as sp]
            [matchete.core :as m]))

(require '[babashka.pods :as pods])
(pods/load-pod 'huahaiy/datalevin "0.9.12")
(require '[pod.huahaiy.datalevin :as d])

(defn gen-dynamic-eid [store]
  (or (some->> (d/q '[:find ?entity :where [?entity _ _]] store)
               (map first) (filter int?) sort last inc) 0))

(defn get-entity [store eid]
  (let [result (d/q '[:find ?attr ?attr-val :in $ ?eid
                      :where [?eid ?attr ?attr-val]] store eid)
        resolved-duplicate-attrs
        (->> result
             (group-by first)
             (map (fn [[attr values]]
                    (if (= (count values) 1)
                      [attr (->> values (map second) first)]
                      [attr (->> values (map second))]))))]
    (if (empty? result) nil (into {} resolved-duplicate-attrs))))

(defn remove-entity [original-store eid]
  (->> (get-entity original-store eid)
       (reduce (fn [store [attr _]]
                 (sp/setval [sp/ALL #(m/match? [eid attr '_] %)] sp/NONE store))
               original-store)))


(defn transform-entity [original-store eid transform-map]
  (->> transform-map
       (reduce (fn [store [attr transformation]]
                 (let [prev-attrs (d/q '[:find ?eid ?attr ?attr-val
                                         :in $ ?eid ?attr
                                         :where [?eid ?attr ?attr-val]]
                                       store eid attr)
                       new-attr-val [eid attr transformation]
                       already-exist? (some #{new-attr-val} prev-attrs)
                       add-duplicate-attr? (and (vector? transformation)
                                                (= (first transformation) :add))]
                   (cond
                     already-exist? store

                     add-duplicate-attr?
                     (->> (distinct (rest transformation))
                          (reduce (fn [store transformation]
                                    (when (fn? transformation) (throw (IllegalStateException. (str "cannot transform value when adding new attribute (" attr ")"))))
                                    (let [new-attr-val [eid attr transformation]]
                                      (if (some #{new-attr-val} prev-attrs) store
                                          (conj store [eid attr transformation]))))
                                  original-store))

                     (empty? prev-attrs)
                     (do (when (fn? transformation) (throw (IllegalStateException. (str "cannot transform value from non-exisiting attribute (" attr ")"))))
                         (conj store new-attr-val))

                     (nil? transformation) store

                     (fn? transformation) (sp/transform [sp/ALL #(m/match? [eid attr '_] %) sp/LAST] transformation store)

                     :else (sp/setval [sp/ALL #(m/match? [eid attr '_] %) sp/LAST] transformation store))))
               original-store)))

(defn overwrite-entity [original-store eid new-entity]
  (-> original-store (remove-entity eid) (transform-entity eid new-entity)))

(defn remove-triples [store clause]
  (sp/setval [sp/ALL #(m/match? clause %)] sp/NONE store))

(defn get-attr [store eid attr]
  (d/q '[:find ?attr-val . :in $ ?eid ?attr
         :where [?eid ?attr ?attr-val]]
       store eid attr))

(defn get-attr-multi [store eid attr]
  (d/q '[:find [?attr-val ...] :in $ ?eid ?attr
         :where [?eid ?attr ?attr-val]]
       store eid attr))

(defn remove-attr [store eid attr]
  (sp/setval [sp/ALL #(m/match? [eid attr '_] %)] sp/NONE store))
