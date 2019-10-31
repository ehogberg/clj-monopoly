(ns clj-monopoly.board
  (:require [clj-monopoly.protocol :refer :all]
            [clj-monopoly.player :refer [buy-property?]]
            [clojure.set :refer [difference]]))

(defrecord EmptySpace [space-name]
  BoardSpace
  (buyable? [_] false)
  (process-space [s game player]
    (println "Processing space.")
    game)
  (to-string [s] (format "Empty space (name: %s)"
                           (:space-name s))))


(defrecord ActionSpace [deck-name]
  BoardSpace
  (buyable? [_] false)
  (process-space [s game player]
    (println "Processing space.")
    game)
  (to-string [s] (format "Deck: %s" (:deck-name s))))


(declare process-street-space board-position-of)


(defn get-player-info [game player-name]
  (get-in game [:players player-name]))


(defn calculate-rent [game property]
  100)


(defn transfer-funds [amount from to game]
  (if (not= from to)
    (do
      (println "Transferring $" amount " from " from " to " to)
      (-> game
          (update-in [:players from :cash] - amount)
          (update-in [:players to :cash] + amount)))
    game))


(defn purchase-space-or-charge-rent
  [{:keys [owner price space-name] :as space}
   {:keys [board] :as game} player]
  (let [{:keys [cash] :as player-info} (get-player-info game player)]
    (if-not owner
      (do
        (println "Can be bought")
        (if (buy-property? player-info space game)
          (let [space-position (board-position-of board space-name)]
            (println (format "%s is buying the property" player))
            (-> game
                (update-in [:players player :cash] - price)
                (assoc-in [:board space-position :owner] player)))
          (do
            (println "Not enough cash to buy property")
            game)))
      (if (not= player owner)
        (do
          (println (format "Rent due to %s" owner))
          (-> game
              (calculate-rent space)
              (transfer-funds player owner game)))
        (do
          (println "No rent due; player owns the property")
          game)))))


(defrecord StreetSpace [space-name price color owner houses mortgaged?]
  BoardSpace
  (buyable? [s] true)
  (process-space [s game player]
    (purchase-space-or-charge-rent s game player))
  (to-string [s] (format "Street: %s" (:space-name s))))


(defn new-street-space [street-name price color]
  (->StreetSpace street-name price color
                 nil 0 false))


(defrecord RailroadSpace [space-name price owner mortgaged?]
  BoardSpace
  (buyable? [_] true)
  (process-space [s game player]
    (purchase-space-or-charge-rent s game player))
  (to-string [s] (format "Railroad: %s" (:space-name s))))


(defn new-railroad-space [railroad-name]
  (->RailroadSpace railroad-name 200 nil false))


(defrecord UtilitySpace [space-name price owner mortgaged?]
  BoardSpace
  (buyable? [_] true)
  (process-space [s game player]
    (purchase-space-or-charge-rent s game player))
  (to-string [s] (format "Utility:" (:space-name s))))


(defn new-utility-space [utility-name]
  (->UtilitySpace utility-name 150 nil false))


(def std-gameboard
  [(->EmptySpace       "Go")
   (new-street-space   "Mediterranean Avenue" 60 :darkpurple)
   (->ActionSpace      "Community Chest")
   (new-street-space   "Baltic Avenue" 60 :darkpurple)
   (->EmptySpace       "Income Tax")
   (new-railroad-space "Reading Railroad")
   (new-street-space   "Oriental Avenue" 100 :lightblue)
   (->ActionSpace      "Chance")
   (new-street-space   "Vermont Avenue" 100 :lightblue)
   (new-street-space   "Connecticut Avenue" 120 :lightblue)
   (->EmptySpace       "Jail")
   (new-street-space   "St. Charles Place" 140 :purple)
   (new-utility-space  "Electric Company")
   (new-street-space   "States Avenue" 140 :purple)
   (new-street-space   "Virginia Avenue" 160 :purple)
   (new-railroad-space "Pennsylvania Railroad")
   (new-street-space   "St. James Place" 160 :orange)
   (->ActionSpace      "Community Chest")
   (new-street-space   "Tennessee Avenue" 160 :orange)
   (new-street-space   "New York Avenue"  200 :orange)
   (->EmptySpace       "Free Parking")
   (new-street-space   "Kentucky Avenue" 220 :red)
   (->ActionSpace      "Chance")
   (new-street-space   "Indiana Avenue" 220 :red)
   (new-street-space   "Illinois Avenue" 240 :red)
   (new-railroad-space "B & O Railroad")
   (new-street-space   "Atlantic Avenue" 260 :yellow)
   (new-street-space   "Ventnor Avenue" 260 :yellow)
   (new-utility-space  "Water Works")
   (new-street-space   "Marvin Gardens" 280 :yellow)
   (->EmptySpace       "Go To Jail")
   (new-street-space   "Pacific Avenue" 300 :green)
   (new-street-space   "North Carolina Avenue" 300 :green)
   (->ActionSpace      "Community Chest")
   (new-street-space   "Pennsylvania Avenue" 320 :green)
   (new-railroad-space "Short Line")
   (->ActionSpace      "Chance")
   (new-street-space   "Park Place" 350 :darkblue)
   (->ActionSpace      "Luxury Tax")
   (new-street-space   "Boardwalk" 400 :darkblue)])


(defn spaces-of-type [board t] (filter #(instance? t %) board))


(defn streets [board] (spaces-of-type board StreetSpace))


(defn railroads [board] (spaces-of-type board RailroadSpace))


(defn streets-by-color [board]
  (reduce
   (fn [m v] (update-in m [(:color v)] conj (:space-name v)))
   {}
   (streets board)))


(defn board-position-of [board property]
  (first (keep-indexed #(if (= property (:space-name %2)) %1 )
                       board)))


(defn find-property [board property-name]
  (->> board
       (filter (comp (partial = property-name) :space-name))
       first))


;; FIXME:  Probably a much more efficient way to do this check.
(defn monopoly? [board street owned-properties]
  (if (instance? StreetSpace street)
    (let [color (:color street)
          all-streets-for-color (set (color (streets-by-color board)))
          owned-property-names (set (map :space-name owned-properties))
          diff (difference all-streets-for-color owned-property-names)]
      (empty? diff))
    false))
