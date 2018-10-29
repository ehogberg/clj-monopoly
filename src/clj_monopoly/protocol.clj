(ns clj-monopoly.protocol)

(defprotocol BoardSpace
  (to-string [s])
  (buyable? [s]))

(defprotocol PlayerIO
  (say [s msg])
  (listen [s]))
