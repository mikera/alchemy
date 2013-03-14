(ns mikera.alchemy.engine
  (:require [mikera.cljutils.find :as find]) 
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:require [mikera.orculje.engine :as en])
  (:import [mikera.engine BitGrid PersistentTreeGrid])
  (:import [mikera.util Rand])
  (:require [mikera.orculje.text :as text]))

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
    (if (:is-hero thing)
      (let [ss (map text/capitalise ss)
           msgs (or (:messages game) [])
           new-msgs (vec (concat msgs ss))
           mlog (or (:message-log game) [msgs])
           new-mlog (assoc mlog 0 new-msgs)]
       (as-> game game
         (assoc game :messages new-msgs)
         (assoc game :message-log new-mlog)))
      game)))


;; ======================================================
;; query functions

(defn hero [game]
  (get-thing game (:hero-id game)))

(defn hero-location ^mikera.orculje.engine.Location [game]
  (:location (hero game)))

(defn the-name [game thing]
  (text/the-name game thing))

(defn a-name [game thing]
  (text/a-name game thing))

(defn base-name [game thing]
  (text/base-name game thing))

(defn check 
  "Random skill check"
  ([a b]
    (> a (* (Rand/nextDouble) (+ a b)))))

(defn is-hostile [a b]
  (or (:is-hero a) (:is-hero b)))

;; =======================================================
;; directions

(def DIRECTIONS
  [(loc -1 -1 0)
   (loc 0 -1 0)
   (loc 1 -1 0)
   (loc 1 0 0)
   (loc 1 1 0)
   (loc 0 1 0)
   (loc -1 1 0)
   (loc -1 0 0)
   (loc 0 0 1)
   (loc 0 0 -1)])

;; =======================================================
;; vision

(defn extend-visibility 
  "Extends visibility by 1 in all directions: needed to see walls / above / below"
  ([^BitGrid bg]
    (.visitSetBits bg (mikera.alchemy.BitGridExtender. bg))))

(defn extend-discovery
  "Extends tile discovery based on current visibility"
  (^PersistentTreeGrid [^BitGrid bg ^PersistentTreeGrid discovered-world ^PersistentTreeGrid world]
    (let [de (mikera.alchemy.DiscoveryExtender. bg discovered-world world)]
      (.visitSetBits bg de)
      (.grid de))))

(def LOS_RAYS 100)
(def RAY_INC 0.33)
(def RAY_LEN 25)

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
    (extend-visibility bg) ;; extend visibility by 1 square in all directions
    (as-> game game
          (assoc game :visibility bg)
          (assoc game :discovered-world (extend-discovery bg (:discovered-world game) (:world game))))))

(defn is-square-visible? [^mikera.orculje.engine.Game game 
                          ^mikera.orculje.engine.Location loc]
  (let [^BitGrid viz (:visibility game)]
    (.get viz (.x loc) (.y loc) (.z loc))))

;; ======================================================
;; identification

(defn identify [game thing]
  (let [name (:name thing)]
    (update-in game [:lib :objects :name :is-identified] (fn [old] true))))

(defn is-identified? [game thing]
  (or (:is-identified thing)
      (:is-identified ((:objects (:lib game)) (:name thing)))))

;; ======================================================
;; actions

(defn wait [game actor]
  (!+ game actor :aps -100))

(defn try-open [game actor door]
  (as-> game game
    (if (? door :is-locked)
      (message game actor (str (text/verb-phrase :the door) " is locked."))
      ((:on-open door) game door actor))    
    (!+ game actor :aps -100)))

(defn try-use [game actor thing]
  (as-> game game
    (if-let [on-use (? thing :on-use)]
      (on-use game thing actor)
      (message game actor (str "You have no idea what to do with " (the-name game thing) ".")))))

(defn try-attack [game thing target]
  (as-> game game
    (message game thing (str (text/verb-phrase :the thing "attack" :the target) "!"))
    (!+ game thing :aps -100)))

(defn try-drop [game actor item]
  (as-> game game
    (message game actor (str (text/verb-phrase :the actor "drop" :the item) "."))
    (move-thing game item (:location actor))
    (!+ game actor :aps -100)))

(defn try-pickup [game actor item]
  (as-> game game
    (message game actor (str (text/verb-phrase :the actor "take" :the item) "."))
    (move-thing game item (:id actor))
    (!+ game actor :aps -100)))

(defn try-bump [game thing target]
  (cond
    (:is-tile target)
      (message game thing (str (text/verb-phrase :the thing "run") " into a wall."))
    (and (:is-creature target) (is-hostile thing target))
      (try-attack game thing target)
    (and (:is-intelligent thing) (:is-door target))
      (try-open game thing target)
    :else
      (try-use game thing target)))

(defn try-move
  [game thing loc]
  (if-let [target (get-blocking game loc)]
    (try-bump game thing target)
    (as-> game game
      (!+ game thing :aps -100)    
      (move-thing game thing loc)
      (if-let [items (and (:is-hero thing) (seq (filter :is-item (get-things game loc))))]
        (message game thing (str "There is " 
                                 (text/and-string (map (partial a-name game) items)) 
                                 " here."))
        game))))

;; ===================================================
;; "AI"

(defn monster-action 
  "Performs one action for a monster. Assume this function only called if monster has 
   sufficient aps to make a move." 
  ([game m]
    ;; (println (str "monster thinking: " (:name m)))
    (let [m (get-thing game m)
          loc (location game m)]
      (if (is-square-visible? game loc)
        (let [hloc (hero-location game)
              dir (direction loc hloc)
              tloc (loc-add loc dir)]
          (try-move game m tloc))
        (if (Rand/chance 0.3)
          (try-move game m (loc-add loc (DIRECTIONS (Rand/r 8))))
          game)))))