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
;; query functions

(defn hero [game]
  (get-thing game (:hero-id game)))

(defn hero-location [game]
  (:location (hero game)))

;; ======================================================
;; action functions



;; ======================================================
;; key external functions (called by main namespace)

(defn new-game []
  (as-> (empty-game) game
    (lib/setup game)
    (dungeon/generate game)
    (add-thing game (loc 0 0 0) (lib/create game "you")) 
    (merge game {:turn 0
                 :hero-id (:last-added-id game)})))

(defn end-turn 
  "Called to update the game after every player turn"
  ([game]
    (as-> game game
      (assoc game :turn (inc (:turn game))))))

(defn handle-move 
  "Handles a hero move"
  [game dir]
  (let [h (hero game)]
    (as-> game game
      (engine/try-move game h (loc-add (:location h) dir))
      (end-turn game))))

(defn handle-command
  "Handles a command, expressed as a complete command String"
  [game k]
  (as-> game game
    (cond
      :else (do 
              (println (str "Turn " (:turn game) " unhandled command [" k "]")) 
              game))    
    (end-turn game)))