(ns clj-monopoly.protocol)

(defprotocol BoardSpace
  (to-string [s])
  (process-space [s game player])
  (rent-due [s game])
  (buyable? [s]))

(defprotocol PlayerIO
  (say [s msg])
  (listen [s]))
