(ns clj-monopoly.game
  (:require [clj-monopoly.board :refer [std-gameboard]]
            [clj-monopoly.player :refer [new-player]]))

(defrecord Game [players current-player board])

(def current-game (ref (->Game {} nil std-gameboard)))

(defn new-game []
  (dosync (ref-set current-game (->Game {} nil std-gameboard))))

(defn add-player [game-piece]
  (dosync
   (alter current-game
          update-in [:players] assoc game-piece (new-player game-piece))))

(defn active-players []
  (filter (fn [[k v]] (not (:bankrupt? v)))
          (:players @current-game)))

(defn mark-bankrupt [game-piece]
  (dosync
   (alter current-game
          update-in [:players game-piece] assoc :bankrupt? true)))

(comment
  (new-game)
  (add-player :thimble)
  (:players @current-game)
  (mark-bankrupt :thimble)
  (active-players)
  )



