(ns clj-monopoly.player)

(defrecord Player [piece cash properties board-position bankrupt?])

(defn new-player [piece] (->Player piece 1500 [] 0 false))

(defn player-owns [game player]
  (filter (comp (partial = player) :owner)
          (:board game)))


(defmulti buy-property? (fn [p _ _] (get p :play-strategy)))

(defmethod buy-property? :default [{:keys [cash]} {:keys [price]} _]
  (println "Cash: " cash ", Price: " price)
  (> cash price))
