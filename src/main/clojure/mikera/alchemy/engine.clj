(ns mikera.alchemy.engine
  (:require [mikera.cljutils.find :as find]) 
  (:use mikera.orculje.core)
  (:require[ mikera.orculje.text :as text]))

(defn message
  "Send a message to a given thing. Should be displaiyed iff it is the hero."
  ([game thing & ss]
    (let [
          msgs (or (:messages game) [])
          new-msgs (vec (concat msgs ss))
          mlog (or (:message-log game) [msgs])
          new-mlog (assoc mlog 0 new-msgs)]
      (as-> game game
        (assoc game :messages new-msgs)
        (assoc game :message-log new-mlog)))))

(defn get-blocking 
  "Gets the object blocking a specific square"
  ([game loc]
    (let [ts (get-things game loc)
          tl (get-tile game loc)]
      (or 
        (find/find-first :is-blocking ts)
        (if (:is-blocking tl) tl nil))))) 

(defn try-move
  [game thing loc]
  (if (get-blocking game loc)
    (message game thing (text/verb-phrase :the thing "run") " into a wall")
    (move-thing game thing loc)))