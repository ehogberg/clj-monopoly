(ns clj-monopoly.player)

(defrecord Player [piece cash properties board-position bankrupt?])

(defn new-player [piece] (->Player piece 1500 [] 0 false))


