(ns mikera.alchemy.world
  (:require [mikera.alchemy.engine :as engine])
  (:require [mikera.alchemy.lib :as lib]) 
  (:require [mikera.cljutils.find :as find]) 
  (:require [mikera.alchemy.dungeon :as dungeon]) 
  (:use mikera.orculje.core))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ======================================================
;; WORLD SIMULATION
;; everything internal to the game goes in here

;; constants

(def BLANK_TILE (thing lib/BLANK_TILE_PROPS))


;; ======================================================
;; key external functions (called by main namespace)

(defn new-game []
  (as-> (empty-game) game
    (lib/setup game)
    (dungeon/generate game)
    (add-thing game (loc 0 0 0) (lib/create game "you")) 
    (merge game {:turn 0
                 :hero-id (:last-added-id game)})
    (engine/update-visibility game)))

(defn monster-turn [game aps-added]
  ;; (println (str "Monster turn: " (:turn game)))
  (loop [game game
         obs (seq (all-things game))]
    (if obs
      (let [o (get-thing game (first obs))]
        (if-let [mfn (:on-action o)]
          (let [new-aps (+ (:aps o) aps-added)
                game (! game o :aps new-aps)]
            ;; (println o)
            (if 
              (and (> new-aps 0) true)
              (recur (mfn game o) (next obs))
              (recur game (next obs))))
          (recur game (next obs))))
      game)))

(defn end-turn 
  "Called to update the game after every player turn"
  ([game]
    (as-> game game
      (monster-turn game (- (:aps (engine/hero game))))
      (monster-turn game 0)
      (monster-turn game 0)
      (engine/update-visibility game)
      (let [turn (:turn game)]
        ;; (println (str "Finished turn: " turn))
        (assoc game :turn (inc turn))))))

(defn handle-move 
  "Handles a hero move"
  [game dir]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/try-move game h (loc-add (:location h) dir))
      (end-turn game)
      (! game h :aps 0))))

(defn handle-command
  "Handles a command, expressed as a complete command String"
  [game k]
  (as-> game game
    (engine/clear-messages game)
    (cond
      :else (do 
              (println (str "Turn " (:turn game) " unhandled command [" k "]")) 
              game))    
    (end-turn game)))