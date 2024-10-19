(ns engine2.try-datalevin)

(require '[babashka.fs :as fs]
         '[engine2.timeline :as t :refer [timeline-schema entity]]
         '[model.topaz :as topaz]
         '[pod.huahaiy.datalevin :as d]
         '[com.rpl.specter :as sp])

(defonce random-db? (atom false))

(defonce last-rand (atom 0))

(defn datasource []
  (if @random-db?
    (str "tmp/random/rpg-" @last-rand)
    "tmp/rpg"))

(defn with-datasource [data-fn]
  (let [timeline (d/get-conn (datasource) timeline-schema)]
    (try (data-fn timeline)
         (finally (d/close timeline)))))

(comment
  (swap! random-db? not)

  @random-db?

  (#_:init-rand-db
   let [rand-id  (rand 42)
        _        (reset! last-rand rand-id)
        database (datasource)
        timeline (d/get-conn database timeline-schema)]
   (try (println "init timeline on" database)
        (finally (d/close timeline))))

  (#_:query-all
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try (d/q '[:find ?a ?b ?c :where [?a ?b ?c]] (d/db timeline))
        (finally (d/close timeline))))

  (#_:query-last-moment
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try (->> (d/q '[:find ?last-epoch .
                    :where [?a :moment.attr/epoch ?last-epoch]
                    :order-by [?last-epoch :desc]]
                  (d/db timeline)))
        (finally (d/close timeline))))

  (#_:transient-op
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try (let [transient-t (d/q '[:find ?eid ?attr ?attr-val
                                 :where
                                 [?eid ?attr ?attr-val]
                                 [?moment :moment.attr/epoch 1]
                                 [?moment :moment.attr/entities ?eid]]
                               (d/db timeline))]
          transient-t)
        (finally (d/close timeline))))

  (let [timeline (d/get-conn "/tmp/random/maybe-bug")
        biggest-value 100]
    (try (d/transact! timeline
                      [[:db/add 1 :value 0]
                       [:db/add 2 :value biggest-value]])
         (println
          [biggest-value "="
           (d/q '[:find ?value
                  :where [?a :value ?value]
                  :order-by [?value :desc]]
                (d/db timeline))])
         (finally (d/close timeline))))

  (fs/delete-tree "/tmp/random")

  (#_:transact-weird-stuff
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try #_(d/transact! timeline [[:db/add "poison" :rule/name :debuff/charm]
                                 [:db/add "poison" :rule/activation '[:find]]
                                 [:db/add "poison" :rule/unleash-fn 'engine2.try-datalevin/deal-with]])
        (let [{:rule/keys [rule-name activation rule-fn]} (d/touch (d/entity (d/db timeline) [:rule/rule-name :debuff/poison]))
              unleash                                     (eval rule-fn)]
          [rule-name activation rule-fn (unleash 1)])
        (finally (d/close timeline))))



  (#_:query-specific-moment
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try (let [epoch          0
              moment         (d/q '[:find ?moment ?attr ?attr-val
                                    :where
                                    [?moment :moment.attr/epoch ?epoch]
                                    [?moment ?attr ?attr-val]
                                    :in $ ?epoch]
                                  (d/db timeline) epoch)
              entities       (d/q '[:find ?eid ?attr ?attr-val
                                    :in $ ?epoch :where
                                    [?moment :moment.attr/epoch ?epoch]
                                    [?moment :moment.attr/entities ?eid]
                                    [?eid ?attr ?attr-val]]
                                  (d/db timeline) epoch)
              effects        (d/q '[:find ?eff-id ?attr ?attr-val
                                    :in $ ?epoch :where
                                    [?moment :moment.attr/epoch ?epoch]
                                    [?moment :moment.attr/entities ?eid]
                                    [?eid :actor.attr/effects ?eff-id]
                                    [?eff-id ?attr ?attr-val]]
                                  (d/db timeline) epoch)
              transient-time (-> moment (into entities) (into effects))]
          transient-time)
        (finally (d/close timeline))))

  (#_:get-specific-moment
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try (->> (t/q-moment timeline 3)
             (sp/setval [(entity :char/hilda) :actor.attr/effects sp/AFTER-ELEM] "trying this")
             (sp/setval [(entity :char/hilda) :actor.attr/effects sp/AFTER-ELEM] "new effect"))
        (finally (d/close timeline))))



  (let [timeline (d/get-conn (datasource) timeline-schema)]
    (try (t/q-moment (d/get-conn (datasource) timeline-schema) 2)
         (finally (d/close timeline))))

  (let [timeline (d/get-conn (datasource) timeline-schema)]
    (try (t/q-last-moment (d/get-conn (datasource) timeline-schema))
         (finally (d/close timeline))))

  (add-tap #(def last-tap %))

  (#_:query-any
   let [timeline         (d/get-conn (datasource) timeline-schema)
        activation-compl [['?s.current-moment :moment.attr/epoch 0]
                          :in '$ '?s.who-acts '?s.timing]
        query-stmt       (apply conj '[:find [?affected-id ?source-id]
                                       :where
                                       [(= ?s.timing :timing/before-action)]
                                       [(= ?s.who-acts ?actor-name)]
                                       [?s.current-moment :moment.attr/entities ?affected-id]
                                       [?affected-id :actor.attr/name ?actor-name]
                                       [?affected-id :actor.attr/effects ?eff-id]
                                       [?eff-id :effect.attr/effect-name :debuff/poison]
                                       [?eff-id :effect.attr/source ?source-id]]
                                activation-compl)]
   (try (d/q query-stmt
             (d/db timeline) :char/aluxes :timing/before-action)
        (finally (d/close timeline))))

  (let [timeline (d/get-conn (datasource) timeline-schema)]
    (try (t/engrave! timeline topaz/world topaz/history)
         (finally (d/close timeline))))

  (#_:query-all
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try (d/q '[:find ?a ?b ?c :where [?a ?b ?c]] (d/db timeline))
        (finally (d/close timeline))))

  (fs/delete-tree "tmp/rpg")
  (fs/delete-tree "tmp/random")

  (#_weird-behaviour
   let [db-name "tmp/random/stuck"
        _       (fs/delete-tree "tmp/random/stuck")
        schema  {:entities {:db/cardinality :db.cardinality/many
                            :db/valueType   :db.type/ref
                            :db/isComponent true}}
        conn    (d/get-conn db-name schema)
        rules   '[[(rich? ?e ?rich)
                   [?e :entities ?a] [?a :name ?rich] [?a :money 100]]]]
   (try (d/transact! conn [{:turn     0
                            :entities [{:name  "Subaru"
                                        :money 100}
                                       {:name  "Emilia"
                                        :money -100}]}
                           {:turn     1
                            :entities [{:name  "Subaru"
                                        :money -100}
                                       {:name  "Emilia"
                                        :money 100}]}])
        (d/q '[:find [?rich ?broke]
               :in $ % ?turn
               :where [?e :turn ?turn]
               (rich? ?e ?rich)
               #_#_#_[?e :entities ?a] [?a :name ?rich] [?a :money 100]
               [?e :entities ?b] [?b :name ?broke] [?b :money -100]]
             (d/db conn) rules 1)
        (finally (d/close conn))))

  (#_will-hang
   let [db-name "tmp/random/stuck"
        schema  {:entities {:db/cardinality :db.cardinality/many
                            :db/valueType   :db.type/ref
                            :db/isComponent true}}
        conn    (d/get-conn db-name schema)]
   (try (d/q '[:find ?a ?b ?c
               :where
               [?a ?b ?c]
               (or [(= ?c "Emilia")]
                   [(= ?c "Beatrice")])] (d/db conn))
        (finally (d/close conn))))

  (let [db-name "tmp/random/stuck"
        schema  {:entities {:db/cardinality :db.cardinality/many
                            :db/valueType   :db.type/ref
                            :db/isComponent true}}
        conn    (d/get-conn db-name schema)]
    (try (d/q '[:find ?a ?b ?c :where [?a ?b ?c]] (d/db conn))
         (finally (d/close conn))))
  )
