(ns mikera.alchemy.world
  (:require [mikera.alchemy.engine :as engine])
  (:require [mikera.alchemy.lib :as lib]) 
  (:require [mikera.cljutils.find :as find]) 
  (:require [mikera.alchemy.dungeon :as dungeon]) 
  (:import [mikera.engine PersistentTreeGrid]) 
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
    ;; build game environment
    (lib/setup game)
    (dungeon/generate game)
    
    ;; special data structures
    (assoc game :functions {:is-identified? engine/is-identified?})
    (assoc game :discovered-world (PersistentTreeGrid.))
    
    ;; add the hero
    (add-thing game (loc 0 0 0) (lib/create game "you")) 
    (merge game {:turn 0
                 :hero-id (:last-added-id game)})
    (reduce 
      (fn [game _] (add-thing game (engine/hero game) (lib/create game "[:is-potion]")))
      game
      (range 100)) 
    
    ;; testing state
    ;; (add-thing game (engine/hero game) (lib/create game "invincibility")) 

    ;; final prep
    (engine/message game (engine/hero game) "Welcome to the dungeon, Alchemist! Seek the Philosopher's Stone!")
    (engine/update-visibility game)))

(defn monster-turn [game aps-added]
  ;; (println (str "Monster turn: " (:turn game)))
  (loop [game game
         obs (seq (all-things game))]
    (if obs
      (if-let [o (get-thing game (first obs))]
        (if-let [mfn (:on-action o)]
          (recur
            (let [game (if (== aps-added 0) game (!+ game o :aps aps-added))]
	            ;; (println (str new-aps " aps action on " (into {} o)))
	            (loop [game game max-moves 10]
                (if-let [o (get-thing game o)]
                  (if (and (> max-moves 0) (>= (:aps o) 0))
                    (recur (mfn game o) (dec max-moves))
                    (if (> (:aps o) 0) ;; check to remove any remaining aps
                      (! game o :aps 0) 
                      game)))))
	            
            (next obs))
          (recur game (next obs)))
        (recur game (next obs)))
      game)))

(defn end-turn 
  "Called to update the game after every player turn"
  ([game]
    (let [hero (engine/hero game)
          aps-debt (- (:aps hero))
          turn (:turn game)]
      ;; (println (str "turn " turn " ending with aps used: " aps-debt))
      (as-> game game
            (monster-turn game aps-debt)
            (engine/update-visibility game)
            (assoc game :turn (inc turn))
            (! game hero :aps 0)
            ;;(do (println "Turn ended") game)
            ))))


(defn handle-move 
  "Handles a hero move"
  [game dir]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/try-move game h (loc-add (:location h) dir))
      (end-turn game))))

(defn handle-drop 
  "Handles an item drop"
  [game item]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/try-drop game h item)
      (end-turn game))))

(defn handle-consume 
  "Handles consuming an item (eating or quaffing)"
  [game item]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/try-consume game h item)
      (end-turn game))))

(defn handle-open 
  "Handles opening a door / lever"
  [game dir]
  (let [game (engine/clear-messages game)
        h (engine/hero game)
        tloc (loc-add (:location h) dir)
        target (find/find-first :on-open (get-things game tloc))
        blocker (get-blocking game tloc)
        ]
    (cond 
      (nil? target)
        (engine/message game h "There is nothing here to open.")
      (and blocker (not= blocker target))
        (engine/message game h (str "Can't open or close " (engine/the-name game target) ": area blocked"))
      :else
        (as-> game game
            (engine/try-open game h target)
            (end-turn game)))))

(defn handle-pickup 
  "Handles item pickup"
  [game item]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/try-pickup game h item)
      (end-turn game))))

(defn handle-wait 
  "Handles waiting"
  [game time]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/message game h "You wait....")
      (!+ game h :aps (- time))
      (end-turn game))))

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