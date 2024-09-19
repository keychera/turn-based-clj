(ns battle-web
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [common :refer [htmx? query->map]]
            [hiccup2.core :refer [html]]
            [selmer.parser :refer [render-file]]
            [timeline :refer [history initial-state reduce-timeline moment]]))

(defn timeline-html
  ([moment#] (timeline-html moment# 0))
  ([moment# instant#]
   (let [timeline (reduce-timeline initial-state @history moment#)
         timeline-per-moment (->> timeline (group-by :state/moment))
         curr-instants (get timeline-per-moment moment#)
         prev-moments (->> (dissoc timeline-per-moment moment#) (map (fn [[k v]] [k v])) (sort-by first >))
         last-moment? (= (inc instant#) (count curr-instants))
         [_ prev-moments] (last prev-moments)]
     (str (html [:div {:id "timeline" :hx-push-url "true" :hx-target "#timeline"}
                 [:div {:hx-get (if last-moment?
                                  (str "/timeline/" (inc moment#) "?moment=0")
                                  (str "/timeline/" moment# "?moment=" (inc instant#)))
                        :hx-trigger "keyup[keyCode==40] from:body"}]
                 (when (> moment# 0)
                   [:div {:hx-get (if (= instant# 0)
                                    (str "/timeline/" (dec moment#) "?moment=" (dec (count prev-moments)))
                                    (str "/timeline/" moment# "?moment=" (dec instant#)))
                          :hx-trigger "keyup[keyCode==38] from:body"}])
                 [:p "Current moment " moment#]
                 (let [viewed-moments (take (inc instant#) curr-instants)
                       last-moment (last viewed-moments)
                       {:state/keys [entities]} last-moment]
                   [:div
                    [:p (str "Moment #" (or moment# 0))]
                    [:p (str entities)]
                    [:ol (->> viewed-moments
                              (map (fn [{:state/keys [desc]}] [:li  [:p [:b (str desc)]]])))]])
                 (->> prev-moments
                      (map (fn [[moment# instants]]
                             (let [last-moment (last instants)
                                   {:state/keys [entities]} last-moment]
                               [:div
                                [:p (str "Moment #" (or moment# 0))]
                                [:p (str entities)]
                                [:ol (->> instants (map (fn [{:state/keys [desc]}] [:li  [:p [:b (str desc)]]])))]]))))])))))

(defn battle-html [moment#]
  (render-file "battle.html" {:timeline-html (timeline-html moment#)}))

(defn get-timeline [req moment]
  (let [moment# (Integer/valueOf moment)
        instant# (or (some->> (:query-string req) query->map :moment Integer/valueOf) 0)]
    (if (htmx? req)
      (timeline-html moment# instant#)
      (battle-html moment#))))

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

  @moment
  (swap! moment inc)
  (add-tap #(def last-tap %))
  (add-tap println)
  last-tap


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

  (stop-all-fn))
