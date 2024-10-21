(ns battle-web
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [common :refer [htmx? query->map]]
            [engine2.try-datalevin :refer [with-datasource]]
            [hiccup2.core :refer [html]]
            [pod.huahaiy.datalevin :as d]
            [selmer.parser :refer [render-file]]
            [engine2.timeline :as t]
            [com.rpl.specter :as sp]))

(defn timeline-html
  ([turn#] (timeline-html turn# 0))
  ([turn# _moment#]
   (let [moment-ids        (with-datasource
                             (fn [timeline]
                               (d/q '[:find ?moment ?epoch
                                      :in $ ?last-turn :where
                                      [?moment :moment.attr/epoch ?epoch]
                                      [(<= ?epoch ?last-turn)]
                                      :order-by [?epoch :desc]
                                      :limit 10]
                                    (d/db timeline) turn#)))
         last-turn?        (= turn# (with-datasource (fn [timeline] (t/q-last-epoch timeline))))
         [current & prevs] moment-ids
         current-moment    (with-datasource (fn [timeline] (d/pull (d/db timeline) '[*] (first current))))
         prev-moments      (with-datasource
                             (fn [timeline]
                               (->> prevs
                                    (mapv (fn [[id _]]
                                            (d/pull (d/db timeline)
                                                    '[:moment.attr/epoch :moment.attr/facts :moment.attr/entities] id))))))]
     (str (html [:div {:id          "timeline"
                       :hx-push-url "true"
                       :hx-target   "#timeline"
                       :hx-swap     "outerHTML"}
                 (when-not last-turn?
                   [:div {:hx-get     (str "/timeline/" (inc turn#))
                          :hx-trigger "keyup[keyCode==40] from:body"}])
                 (when (> turn# 0)
                   [:div {:hx-get     (str "/timeline/" (dec turn#))
                          :hx-trigger "keyup[keyCode==38] from:body"}])
                 [:p "Current moment " turn#]
                 (let [entities (:moment.attr/entities current-moment)
                       desc     (sp/select [:moment.attr/facts sp/ALL :fact/desc some?] current-moment)]
                   [:div
                    [:p (str "Moment #" (or turn# 0))]
                    [:p [:strong (str desc)]]
                    [:p (str entities)]])
                 (->> prev-moments
                      (map (fn [{:moment.attr/keys [epoch entities facts]}]
                             (let [desc (sp/select [sp/ALL :fact/desc some?] facts)]
                               [:div
                                [:p (str "Moment #" epoch)]
                                [:p [:strong (str desc)]]
                                [:p (str entities)]]))))])))))

(defn battle-html [moment#]
  (render-file "battle.html" {:timeline-html (timeline-html moment#)}))

(defn get-timeline [req moment]
  (let [turn# (Integer/valueOf moment)
        moment# (or (some->> (:query-string req) query->map :moment Integer/valueOf) 0)]
    (if (htmx? req)
      (timeline-html turn# moment#)
      (battle-html turn#))))

(defn router [req]
  (let [paths (some-> (:uri req) (str/split #"/") rest vec)
        verb  (:request-method req)]
    (match [verb paths]
      [:get  []]                {:body (battle-html 0)}
      [:get  ["timeline" moment]] {:body (get-timeline req moment)}
      :else {:status 404
             :body   "not found"})))

(comment
  (require 'panas.reload 'panas.default)

  (add-tap #(def last-tap %))
  (add-tap #(println %))

  (def stop-all-fn
    (let [watch-dir "src" url "localhost" port 4242
          root-url (str "http://" (or url "0.0.0.0") ":" (or port 8090))
          stop-server-fn (panas.reload/panas-server #'router
                                                    {:url  url
                                                     :port port}
                                                    {})
          stop-watcher-fn (panas.reload/run-file-watcher root-url router watch-dir)]
      (println "[panas] serving" root-url)
      (fn []
        (stop-server-fn) (stop-watcher-fn))))

  (router {:request-method :get :uri "/timeline/1"})

  (with-datasource
    (fn [timeline] (d/pull (d/db timeline) '[*] 27)))

  (stop-all-fn))
