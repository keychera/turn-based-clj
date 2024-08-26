(ns battle
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [common :refer [payload->map]]
            [hiccup2.core :refer [html]]
            [timeline :refer [reduce-timeline turn]]
            [selmer.parser :refer [render-file]]))

(defn battle-html [_]
  (let [current-turn @turn]
    (render-file "battle.html"
                 {:turn current-turn
                  :timeline (reduce-timeline current-turn)})))

(defn post-battle-state [req]
  (let [{:strs [direction]} (payload->map req)
        new-turn (swap! turn (if (= direction "up") dec inc))]
    (str (html [:p "Turn " new-turn]
               [:ol (->> (reduce-timeline new-turn)
                         (map (fn [{:keys [desc state]}]
                                [:li
                                 [:p (str state)]
                                 [:p (str desc)]])))]))))

(defn router [req]
  (let [paths (some-> (:uri req) (str/split #"/") rest vec)
        verb  (:request-method req)]
    (match [verb paths]
      [:get []]         {:body (battle-html req)}
      [:post ["state"]] {:body (post-battle-state req)}
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
