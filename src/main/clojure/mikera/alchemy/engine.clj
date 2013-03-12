(ns mikera.alchemy.engine
  (:require [mikera.cljutils.find :as find]) 
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:require [mikera.orculje.engine :as en])
  (:import [mikera.engine BitGrid])
  (:require[ mikera.orculje.text :as text]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; message logging

(def LOG_LENGTH 100)

(defn clear-messages [game]
  (if (seq (:messages game))
    (as-> game game
          (assoc game :messages [])
          (assoc game :message-log (vec (take LOG_LENGTH (cons [] (:message-log game))))))
    game))

(defn message
  "Send a message to a given thing. Should be displayed iff it is the hero."
  ([game thing & ss]
    (let [ss (map text/capitalise ss)
          msgs (or (:messages game) [])
          new-msgs (vec (concat msgs ss))
          mlog (or (:message-log game) [msgs])
          new-mlog (assoc mlog 0 new-msgs)]
      (as-> game game
        (assoc game :messages new-msgs)
        (assoc game :message-log new-mlog)))))


;; ======================================================
;; query functions

(defn hero [game]
  (get-thing game (:hero-id game)))

(defn hero-location ^mikera.orculje.engine.Location [game]
  (:location (hero game)))

;; =======================================================
;; vision

(def LOS_RAYS 100)
(def RAY_INC 0.33)
(def RAY_LEN 15)

(defn extend-visibility 
  "Extends visibility by 1 in all directions: needed to see walls / above / below"
  ([^BitGrid bg]
    (.visitSetBits bg (mikera.alchemy.BitGridExtender. bg))))

(defn update-visibility [game]
  (let [bg (BitGrid.)
        hloc (hero-location game)
        hx (.x hloc) hy (.y hloc) hz (.z hloc)]
    (.set bg hx hy hz true)
    (dotimes [i LOS_RAYS]
      (let [dx (Math/sin (* i (/ (* 2.0 Math/PI) LOS_RAYS)))
            dy (Math/cos (* i (/ (* 2.0 Math/PI) LOS_RAYS)))]
        (loop [d 1.0]
          (when (< d RAY_LEN)
            (let [px (int (Math/round (+ hx (* dx d))))
                  py (int (Math/round (+ hy (* dy d))))
                  view-ok? (not (get-pred game (loc px py hz) :is-view-blocking))]
              (when view-ok?
                (.set bg px py hz true)
                (recur (+ d RAY_INC))))))))
    (extend-visibility bg)
    (assoc game :visibility bg)))

(defn is-square-visible? [^mikera.orculje.engine.Game game 
                          ^mikera.orculje.engine.Location loc]
  (let [^BitGrid viz (:visibility game)]
    (.get viz (.x loc) (.y loc) (.z loc))))

;; ======================================================
;; actions

(defn try-attack [game thing target]
  (message game thing (str (text/verb-phrase :the thing "attack" :the target))))

(defn try-bump [game thing target]
  (cond
    (:is-tile target)
      (message game thing (str (text/verb-phrase :the thing "run") " into a wall."))
    (:is-hostile target)
      (try-attack game thing target)
    :else
      (error "Don't know hot to touch: " target)))

(defn monster-action 
  ([game m]
    game))

(defn try-move
  [game thing loc]
  (if-let [target (get-blocking game loc)]
    (try-bump game thing target)
    (move-thing game thing loc)))