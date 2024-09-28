(ns util.test-data)

(def default-initial-moment
  [[:info/moment :moment/turn 0]
   [:info/moment :moment/actors :actor/hilda]
   [:info/moment :moment/actors :actor/aluxes]
   [:actor/hilda :attr/hp 560]
   [:actor/hilda :attr/mp 200]
   [:actor/aluxes :attr/hp 800]
   [:actor/aluxes :attr/mp 10]])

(defn build-history [players history]
  #:battle-data
   {:num-moment-per-turn (count players)
    :actors players
    :history-atom (atom history)})
