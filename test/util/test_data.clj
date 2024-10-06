(ns util.test-data)

(def default-initial-moment
  [[:info/timeline :timeline/turn 0]
   [:info/timeline :timeline/actors :actor/hilda]
   [:info/timeline :timeline/actors :actor/aluxes]
   [:actor/hilda :attr/hp 560]
   [:actor/hilda :attr/mp 200]
   [:actor/aluxes :attr/hp 800]
   [:actor/aluxes :attr/mp 10]])

(defn build-history [players active-effects history]
  #:battle-data
   {:num-actions-per-turn (count players)
    :actors players
    :active-effects active-effects
    :history-atom (atom history)})
