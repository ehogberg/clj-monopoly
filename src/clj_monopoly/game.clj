(ns clj-monopoly.game
  (:require [clj-monopoly.board :refer [std-gameboard]]
            [clj-monopoly.player :refer [new-player]]
            [clj-monopoly.protocol :refer [process-space to-string]]))

(defrecord Game [players play-order board turn-count])

(defn new-game []
   (->Game {} nil std-gameboard 0))

(defn add-player [game game-piece]
  (-> game
      (update :players assoc game-piece
              (new-player game-piece))
      (update :play-order #(conj % game-piece))))


(defn active-players
  "Returns a list of all players who haven't
   yet been marked as bankrupt."
  [game]
  (filter (comp not :bankrupt? val) (:players game)))


(defn mark-bankrupt [game game-piece]
  (update-in game [:players game-piece]
             assoc :bankrupt? true))

(defn game-over?
  "Has a game-over condition been met?"
  [game]
  (cond
   ;;(> (:turn-count game) 50) :out-of-turns
   (< (count (active-players game)) 2) :player
   :else false))


(defn set-play-order
  "Randomly set a turn order for players."
  [game]
  (update game :play-order shuffle))


(defn set-next-player [game]
  (update game :play-order
          #(vec (concat (rest %) [(first %)]))))


(defn increment-turn [game]
  (update game :turn-count inc))


(defn current-player
  "Who is the current player?"
  [game]
  (-> game
      :play-order
      first))


(defn board-size [game] (count (:board game)))


(defn advance-on-board
  "Roll 2 'dice', add the sum to the player's current board position.
   If they overshoot the board spaces, set them to the appropriate
   position after the Go space."
  [game curr-pos]
  (let [roll1 (inc (rand-int 6))
        roll2 (inc (rand-int 6))
        combined-roll (+ roll1 roll2)
        new-board-position (mod (+ curr-pos combined-roll)
                                (board-size game))]
    (println (format "Rolled %d + %d = %d" roll1 roll2 (+ roll1 roll2)))
    new-board-position))


(defn bankruptcy-check [game {:keys [cash piece] :as player}]
  (println "Bankruptcy check:  available cash = " cash)
  (if (neg? cash)
    (mark-bankrupt game piece)
    game))

(defn process-go
  "If the specified player passed Go, pay them $200"
  [game player old-pos new-pos]
  (if (< new-pos old-pos)
    (do
      (println (format "Player %s passed Go; receives $200" player))
      (update-in game [:players player :cash] + 200))
    game))


(defn take-turns [game]
  (if-let [game-over (game-over? game)]
    (do
      (println (format "Game over: %s" game-over))
      game)
    (let [curr-player (current-player game)
          curr-board-position (get-in game
                                      [:players curr-player :board-position])
          new-board-position (advance-on-board game curr-board-position)
          new-space (get-in game [:board new-board-position])
          turn-count  (:turn-count game)]
      (println (format "Turn #%d [%s]" turn-count curr-player))
      (println "Previous board position: " curr-board-position)
      (println "New board position: " new-board-position)
      (println (format "Player landed on: %s" (to-string new-space)))
      (as-> game g
        (assoc-in g [:players curr-player :board-position]
                  new-board-position)
        (process-go g curr-player curr-board-position new-board-position)
        (process-space new-space g curr-player)
        (bankruptcy-check g (get-in g [:players curr-player]))
        (set-next-player g)
        (increment-turn g)
        (recur g)))))


(defn owned-properties [{:keys [board]}]
  (as-> board b
    (group-by :owner b)
    (dissoc b nil)))


(defn player-properties [game player]
  (-> game
      (owned-properties)
      (get player)))


(defn play-game
  "Play a game of Monopoly"
  [game]
  (-> game
      (set-play-order)
      (take-turns)))


(comment
  (let [game (-> (new-game)
                 (add-player :thimble)
                 (add-player :car)
                 (play-game))]
    (clojure.pprint/pprint (player-properties game :car))
    )
  )
