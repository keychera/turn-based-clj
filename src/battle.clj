(ns battle
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [common :refer [payload->map]]
            [hiccup2.core :refer [html]]
            [selmer.parser :refer [render-file]]))


(def battle-state
  {:actors {:hilda {:hp 400 :mp 200}
            :aluxes {:hp 650 :mp 25}}})

(defonce turn (atom 0))

(def nothing {:desc  "nothing happened"
              :alter identity})

(defn attack [actor target damage]
  {:desc (str actor " attacks " target " for " damage " damage!")
   :alter (fn [state] (update-in state [:actors target] #(update % :hp - damage)))})

(def history
  (atom [nothing 
         nothing
         (attack :hilda :aluxes 400)
         (attack :aluxes :hilda 300)
         nothing]))

(defn reduce-timeline [limit]
  (let [history @history]
    (->> history
         (take (min limit (count history)))
         (reduce (fn [timeline {:keys [desc alter]}]
                   (let [{:keys [state]} (last timeline)]
                     (conj timeline {:desc desc
                                     :state (alter state)})))
                 [{:desc "battle begins"
                   :state battle-state}]))))

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
  (require '[panas.reload :refer [panas-server run-file-watcher]]
           '[panas.default :refer [reloadable?]])

  @turn
  (swap! turn inc)
  (add-tap #(def last-tap %))
  (add-tap println)
  last-tap

  (def stop-all-fn
    (let [watch-dir "src" url "localhost" port 4242
          root-url (str "http://" (or url "0.0.0.0") ":" (or port 8090))
          stop-server-fn (panas-server #'router
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
