(ns engine2.try-datalevin)

(require '[babashka.fs :as fs]
         '[engine2.timeline :refer [timeline-schema entity get-moment engrave!]]
         '[model.topaz :as topaz]
         '[pod.huahaiy.datalevin :as d]
         '[com.rpl.specter :as sp])

(defonce random-db? (atom false))

(defonce last-rand (atom 0))

(defn datasource []
  (if @random-db?
    (str "tmp/random/rpg-" @last-rand)
    "tmp/rpg"))

(defn deal-with [it]
  (str it " dealt!"))

(comment
  deal-with
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

  (#_:transact-weird-stuff
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try (d/transact! timeline [[:db/add "poison" :rule/name :debuff/poison]
                               [:db/add "poison" :rule/activation '[:find]]
                               [:db/add "poison" :rule/unleash-fn 'engine2.try-datalevin/deal-with]])
        (let [{:rule/keys [name activation unleash-fn]} (d/touch (d/entity (d/db timeline) [:rule/name :debuff/poison]))
              unleash                                   (eval unleash-fn)]
          [name activation unleash-fn (unleash 1)])
        (finally (d/close timeline))))

  (#_:query-specific-moment
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try (let [epoch    1
              moment   (d/q '[:find ?moment ?attr ?attr-val
                              :in $ ?epoch :where
                              [?moment :moment.attr/epoch ?epoch]
                              [?moment ?attr ?attr-val]]
                            (d/db timeline) epoch)
              entities (d/q '[:find ?eid ?attr ?attr-val
                              :in $ ?epoch :where
                              [?moment :moment.attr/epoch ?epoch]
                              [?moment :moment.attr/entities ?eid]
                              [?eid ?attr ?attr-val]]
                            (d/db timeline) epoch)
              effects  (d/q '[:find ?eff-id ?attr ?attr-val
                              :in $ ?epoch :where
                              [?moment :moment.attr/epoch ?epoch]
                              [?moment :moment.attr/entities ?eid]
                              [?eid :actor.attr/effects ?eff-id]
                              [?eff-id ?attr ?attr-val]]
                            (d/db timeline) epoch)]
          (-> moment (into entities) (into effects)))
        (finally (d/close timeline))))

  (#_:get-specific-moment
   let [timeline (d/get-conn (datasource) timeline-schema)]
   (try (->> (get-moment timeline 3)
             (sp/setval [(entity :char/hilda) :actor.attr/effects sp/AFTER-ELEM] "trying this")
             (sp/setval [(entity :char/hilda) :actor.attr/effects sp/AFTER-ELEM] "new effect"))
        (finally (d/close timeline))))

  (engrave! topaz/history topaz/initial-moment 'model.topaz)

  (fs/delete-tree "tmp/rpg")
  (fs/delete-tree "tmp/random")
  :end)
