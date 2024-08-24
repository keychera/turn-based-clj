(ns battle
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [hiccup2.core :refer [html]]
            [selmer.parser :refer [render-file]]))


(def battle-state
  {:hilda  {:hp 400 :mp 200}
   :aluxes {:hp 650 :mp 25}})

(defonce tick (atom 0))

(defn battle-html [_]
  (render-file "battle.html" {:status battle-state
                              :tick   @tick}))

(defn post-battle-state [_]
  (swap! tick inc)
  (str (html [:div#state {:hx-post "/state" :hx-trigger "updown-event from:body"}
              [:p "Tick " @tick]
              [:p (str battle-state)]])))

(defn router [req]
  (let [paths (some-> (:uri req) (str/split #"/") rest vec)
        verb  (:request-method req)]
    (match [verb paths]
      [:get []]         {:body (battle-html req)}
      [:post ["state"]] {:body (post-battle-state req)}
      :else {:status 404
             :body   "not found"})))


(comment
  (require '[panas.reload :refer [panas-server run-file-watcher]]
           '[panas.default :refer [reloadable?]])

  @tick
  (swap! tick inc)

  (def stop-all-fn
    (let [watch-dir "src" url "localhost" port 4242
          root-url (str "http://" (or url "0.0.0.0") ":" (or port 8090))
          stop-server-fn (panas-server router
                                       {:url  url
                                        :port port}
                                       {:reloadable? (every-pred
                                                      (fn [{{:strs [hx-target hx-request]} :headers}]
                                                        (or (not hx-request) (= "main-body" hx-target)))
                                                      reloadable?)})
          stop-watcher-fn (run-file-watcher root-url router watch-dir)]
      (println "[panas] serving" root-url)
      (fn []
        (stop-server-fn) (stop-watcher-fn))))


  (stop-all-fn))
