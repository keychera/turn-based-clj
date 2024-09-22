(ns util.test-data)

(def default-initial-state
  #:state{:turn 0
          :desc "battle begins"
          :entities #:actor{:hilda #:attr{:hp 560 :mp 200}
                            :aluxes #:attr{:hp 800 :mp 10}}})

(defn build-history [players history]
  #:battle-data
   {:num-moment-per-turn (count players)
    :actors players
    :history-atom (atom history)})
