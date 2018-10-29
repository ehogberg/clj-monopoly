(ns clj-monopoly.game
  (:require [clj-monopoly.board :refer [std-gameboard]]
            [clj-monopoly.player :refer [new-player]]
            [clj-monopoly.protocol :refer [to-string]]))

(defrecord Game [players play-order board turn-count])

(def current-game (ref (->Game {} nil std-gameboard 0)))

(defn new-game []
  (dosync (ref-set current-game (->Game {} nil std-gameboard 0))))

(defn add-player [game-piece]
  (dosync
   (alter current-game update :players assoc game-piece (new-player game-piece))
   (alter current-game update :play-order #(conj % game-piece))))

(defn active-players
  "Returns a list of all players who haven't
   yet been marked as bankrupt."
  []
  (filter (comp not :bankrupt? val) (:players @current-game)))


(defn mark-bankrupt [game-piece]
  (dosync
   (alter current-game
          update-in [:players game-piece] assoc :bankrupt? true)))

(defn game-over?
  "Has a game-over condition been met?"
  []
  (cond
   (> (:turn-count @current-game) 10) :out-of-turns
   (< (count (active-players)) 2)     :player
   :else false)) 


(defn set-play-order
  "Randomly set a turn order for players."
  []
  (dosync
   (alter current-game update :play-order shuffle)))

(defn set-next-player []
  (dosync
   (alter current-game update :play-order
          #(vec (concat (rest %) [(first %)])))))

(defn increment-turn []
  (dosync
   (alter current-game update :turn-count inc)))

(defn current-player
  "Who is the current player?"
  []
  (-> @current-game
      :play-order
      first))


(defn board-size [] (count (:board @current-game)))

(defn advance-on-board [curr-pos]
  (let [roll1 (inc (rand-int 6))
        roll2 (inc (rand-int 6))
        combined-roll (+ roll1 roll2)
        new-board-position (mod (+ curr-pos combined-roll) (board-size))]
    (if (< new-board-position curr-pos)
      (println "Passed Go..."))
    new-board-position))

(defn update-board-position [player pos]
  (dosync
   (alter @current-game update-in [:players player :board-position] pos)))

(defn take-turn []
  (let [game-over (game-over?)
        curr-player (current-player)
        curr-board-position (get-in @current-game [:players curr-player :board-position])
        turn-count  (:turn-count @current-game)]
    (println "Current board position: " curr-board-position)
    (if game-over
      (do
        (println (format "Game over: %s" game-over))
        true)
      (let [new-board-position (advance-on-board curr-board-position)
            new-space (get-in @current-game [:board new-board-position])]
        (println
         (format "Turn #%d [%s]" turn-count (current-player)))
        (println
         (format "Player landed on: %s"
                 (to-string new-space)))
        (update-board-position curr-player new-board-position)
        (set-next-player)
        (increment-turn)
        (take-turn)))))

(defn play-game
  "Play a game of Monopoly"
  []
  (set-play-order)
  (take-turn))

(comment
  (new-game)
  (add-player :thimble)
  (add-player :car)
  (:players @current-game)q
  (mark-bankrupt :thimble)
  (active-players)
  (set-play-order)
  (play-game)
  (:play-order @current-game)

  (get-in @current-game [:players :thimble])
  )



