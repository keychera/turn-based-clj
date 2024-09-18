(ns battle-web
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [common :refer [htmx? query->vec]]
            [hiccup2.core :refer [html]]
            [selmer.parser :refer [render-file]]
            [timeline :refer [history initial-state reduce-timeline turn]]))

(defn timeline-html
  ([turn] (timeline-html turn 0))
  ([turn moment]
   (let [timeline (reduce-timeline initial-state @history turn)
         [current-turn & prev-turns] (->> timeline rseq (group-by :state/turn))]
     (str (html [:div {:id "timeline" :hx-push-url "true" :hx-target "#timeline"}
                 [:div {:hx-get (str "/timeline/" (inc turn)) :hx-trigger "keyup[keyCode==40] from:body"}]
                 (when (> turn 0)
                   [:div {:hx-get (str "/timeline/" (dec turn)) :hx-trigger "keyup[keyCode==38] from:body"}])
                 [:p "Current turn " turn]
                 (->> prev-turns
                      (mapv (fn [[turn moments]]
                              (let [last-moment (peek moments)
                                    {:state/keys [entities]} last-moment]
                                [:div
                                 [:p (str "Turn #" (or turn 0))]
                                 [:p (str entities)]
                                 [:ol (->> moments (map (fn [{:state/keys [desc]}] [:li  [:p [:b (str desc)]]])))]])))
                      rseq)
                 (let [[turn moments] current-turn
                       last-moment (peek moments)
                       {:state/keys [entities]} last-moment]
                   [:div
                    [:p (str "Turn #" (or turn 0))]
                    [:p (str entities)]
                    [:ol (->> moments (map (fn [{:state/keys [desc]}] [:li  [:p [:b (str desc)]]])))]])])))))

(defn battle-html [turn]
  (render-file "battle.html" {:timeline-html (timeline-html turn)}))

(defn get-timeline [req turn]
  (let [turn (Integer/valueOf turn)
        moment (or (some-> (:query-string req) query->vec :moment) 1)]
    (if (htmx? req)
      (timeline-html turn moment)
      (battle-html turn))))

(defn router [req]
  (let [paths (some-> (:uri req) (str/split #"/") rest vec)
        verb  (:request-method req)]
    (match [verb paths]
      [:get  []]                {:body (battle-html 0)}
      [:get  ["timeline" turn]] {:body (get-timeline req turn)}
      :else {:status 404
             :body   "not found"})))


(comment
  (require 'panas.reload 'panas.default)

  @turn
  (swap! turn inc)
  (add-tap #(def last-tap %))
  (add-tap println)
  last-tap


  (def stop-all-fn
    (let [watch-dir "src" url "localhost" port 4242
          root-url (str "http://" (or url "0.0.0.0") ":" (or port 8090))
          stop-server-fn (panas.reload/panas-server #'router
                                                    {:url  url
                                                     :port port}
                                                    {:reloadable? (every-pred
                                                                   (fn [{{:strs [hx-target hx-request]} :headers}]
                                                                     (or (not hx-request) (= "main-body" hx-target)))
                                                                   panas.default/reloadable?)})
          stop-watcher-fn (panas.reload/run-file-watcher root-url router watch-dir)]
      (println "[panas] serving" root-url)
      (fn []
        (stop-server-fn) (stop-watcher-fn))))


  (stop-all-fn))
