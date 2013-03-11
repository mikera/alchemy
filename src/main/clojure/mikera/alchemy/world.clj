(ns mikera.alchemy.world
  (:require [mikera.alchemy.lib :as lib]) 
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
;; key external functions (called by main namespace)

(defn new-game []
  (as-> (empty-game) game
    (lib/setup game) 
    (add-thing game (loc 0 0 0) (lib/create game "you")) 
    (merge game {:turn 0
                 :hero-id (:last-added-id game)})))

(defn handle-command
  "Handles a command, expressed as a complete command String"
  [game k]
  (let [turn (inc (:turn game))]
    (println (str "Handling turn " turn " with keypress " k))
    (-> game
      (assoc :turn turn))))