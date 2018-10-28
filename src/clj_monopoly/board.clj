(ns clj-monopoly.board
  (:require [clj-monopoly.protocol :refer :all]))

(defrecord EmptySpace [label]
  BoardSpace
  (buyable? [_] false)
  (to-string [s] (format "Empty space (label: %s)"
                           (:label s))))

(defrecord ActionSpace [deck-name]
  BoardSpace
  (buyable? [_] false)
  (to-string [s] (format "Deck: %s" (:deck-name s))))

(defrecord StreetSpace [name price color]
  BoardSpace
  (buyable? [s] true)
  (to-string [s] (format "Street: %s" (:name s))))

(defrecord RailroadSpace [name]
  BoardSpace
  (buyable? [_] true)
  (to-string [s] (format "Railroad: %s" (:name s))))

(defrecord UtilitySpace [name]
  BoardSpace
  (buyable? [_] true)
  (to-string [s] (format "Utility:" (:name s))))

(def std-gameboard
  [(->EmptySpace    "Go")
   (->StreetSpace   "Mediterranean Avenue" 60 :darkpurple)
   (->ActionSpace   "Community Chest")
   (->StreetSpace   "Baltic Avenue" 60 :darkpurple)
   (->EmptySpace    "Income Tax")
   (->RailroadSpace "Reading Railroad")
   (->StreetSpace   "Oriental Avenue" 100 :lightblue)
   (->ActionSpace   "Chance")
   (->StreetSpace   "Vermont Avenue" 100 :lightblue)
   (->StreetSpace   "Connecticut Avenue" 120 :lightblue)
   (->EmptySpace    "Jail")
   (->StreetSpace   "St. Charles Place" 140 :purple)
   (->UtilitySpace  "Electric Company")
   (->StreetSpace   "States Avenue" 140 :purple)
   (->StreetSpace   "Virginia Avenue" 160 :purple)
   (->RailroadSpace "Pennsylvania Railroad")
   (->StreetSpace   "St. James Place" 160 :orange)
   (->ActionSpace   "Community Chest")
   (->StreetSpace   "Tennessee Avenue" 160 :orange)
   (->StreetSpace   "New York Avenue"  200 :orange)
   (->EmptySpace    "Free Parking")
   (->StreetSpace   "Kentucky Avenue" 220 :red)
   (->ActionSpace   "Chance")
   (->StreetSpace   "Indiana Avenue" 220 :red)
   (->StreetSpace   "Illinois Avenue" 240 :red)
   (->RailroadSpace "B & O Railroad")
   (->StreetSpace   "Atlantic Avenue" 260 :yellow)
   (->StreetSpace   "Ventnor Avenue" 260 :yellow)
   (->UtilitySpace  "Water Works")
   (->StreetSpace   "Marvin Gardens" 280 :yellow)
   (->EmptySpace    "Go To Jail")
   (->StreetSpace   "Pacific Avenue" 300 :green)
   (->StreetSpace   "North Carolina Avenue" 300 :green)
   (->ActionSpace   "Community Chest")
   (->StreetSpace   "Pennsylvania Avenue" 320 :green)
   (->RailroadSpace "Short Line")
   (->ActionSpace   "Chance")
   (->StreetSpace   "Park Place" 350 :darkblue)
   (->ActionSpace   "Luxury Tax")
   (->StreetSpace   "Boardwalk" 400 :darkblue)])

(defn current-gameboard [] std-gameboard)

(defn of-type [t] (filter #(instance? t %) (current-gameboard)))

(defn streets [] (of-type StreetSpace))

(defn streets-by-color []
  (->> (streets)
       (reduce (fn [m v] (update-in m [(:color v)] conj (:name v) )) {})))

(defn monopoly? [street owned-properties]
  (let [color (:color street)
        all-streets-for-color (set (color (streets-by-color)))
        owned-property-names (set (map :name owned-properties))
        diff (clojure.set/difference all-streets-for-color owned-property-names)]
    (empty? diff)))




