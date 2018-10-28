(ns clj-monopoly.protocol)

(defprotocol BoardSpace
  (to-string [s])
  (buyable? [s]))
