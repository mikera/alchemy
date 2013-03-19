(ns mikera.alchemy.world
  (:require [mikera.alchemy.engine :as engine])
  (:require [mikera.alchemy.lib :as lib]) 
  (:require [mikera.cljutils.find :as find]) 
  (:use [mikera.cljutils.error]) 
  (:require [mikera.alchemy.dungeon :as dungeon]) 
  (:require [mikera.orculje.mapmaker :as mm]) 
  (:import [mikera.engine PersistentTreeGrid]) 
  (:import [mikera.util Rand]) 
  (:use mikera.orculje.core))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; ======================================================
;; WORLD SIMULATION
;; everything internal to the game goes in here

;; constants

(def BLANK_TILE (thing lib/BLANK_TILE_PROPS))
(def UNSEEN_TILE (thing lib/UNSEEN_TILE_PROPS))

(def WANDERING_MONSTER_RATE 5000) ;; number of ticks per monster add

;; ======================================================
;; key external functions (called by main namespace)

(defn new-game []
  (as-> (empty-game) game
    ;; build game environment
    (assoc game :max-level 1)
    (lib/setup game)
    (dungeon/generate game)
    
    ;; special data structures
    (assoc game :functions {:is-identified? engine/is-identified?
                            :create lib/create})
    (assoc game :discovered-world (PersistentTreeGrid.))
    
    ;; add the hero
    (let [start-location (or (:start-location game) (loc 0 0 0))]
      (add-thing game start-location (lib/create game "you"))) 
    (merge game {:turn 0              
                 :hero-id (:last-added-id game)})
    (reduce 
      (fn [game _] 
        (as-> game game
          (add-thing game (engine/hero game) (lib/create game "[:is-ingredient]" 2))
          (add-thing game (engine/hero game) (lib/create game "[:is-potion]" 2))
          ))
      game
      (range (Rand/d 5))) 
    (engine/identify-all game identity (contents (engine/hero game)))
    
    ;; testing state
    ;; (add-thing game (engine/hero game) (lib/create game "invincibility")) 

    ;; final prep
    (engine/message game (engine/hero game) "Welcome to the dungeon, Alchemist! Seek the Philosopher's Stone!")
    (engine/message game (engine/hero game) "Press ? to show available key commands")
    (engine/update-visibility game)))

(defn monster-turn [game aps-added]
  ;; (println (str "Monster turn: " (:turn game)))
  (loop [game game
         obs (seq (all-things game))]
    
    (if (seq obs)
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
                      game))
                  game)))
            (next obs))
          (recur game (next obs)))
        (recur game (next obs)))
      game)))

(defn maybe-add-wandering-monsters 
  ([game aps-debt]
    (let [num (Rand/po aps-debt WANDERING_MONSTER_RATE)
          lmin (:min (:volume game))
          lmax (:max (:volume game))]
      (reduce
        (fn [game _]
          (if-let [l (mm/find-loc lmin lmax 
                               #(and 
                                  (not (get-blocking game %))
                                  (not (engine/is-square-visible? game %))))]
            (let [m (lib/create game "[:is-creature]" (:max-level game))]
                (dungeon/maybe-place-thing game l l m))
            game))
        game
        (range num)))))

(defn end-turn 
  "Called to update the game after every player turn"
  ([game]
    (let [hero (engine/hero game)
          hloc (engine/hero-location game)
          aps-debt (- (:aps hero))
          turn (:turn game)]
      ;; (println (str "turn " turn " ending with aps used: " aps-debt))
      (as-> game game
            (maybe-add-wandering-monsters game aps-debt)
            (monster-turn game aps-debt)
            (engine/update-visibility game)
            (assoc game :turn (inc turn))
            (! game hero :aps 0)
            (assoc game :max-level (max (- (.z hloc)) (:max-level game)))
            ;;(do (println "Turn ended") game)
            ))))

(defn message 
  "Handles a message to the hero"
  [game msg]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/message game h msg)
      (end-turn game))))

(defn handle-move 
  "Handles a hero move"
  [game dir]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/try-move-dir game h dir)
      (end-turn game))))

(defn handle-drop 
  "Handles an item drop"
  [game item]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/try-drop game h item)
      (end-turn game))))

(defn handle-throw 
  "Handles an item throw / missile fire"
  [game item tloc]
  (let [hero (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (if (engine/is-square-visible? game tloc)
        (engine/try-throw game hero tloc item)
        (engine/message game "You can't see that location..."))
      (end-turn game))))


(defn handle-consume 
  "Handles consuming an item (eating or quaffing)"
  [game item]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/try-consume game h item)
      (end-turn game))))


(defn handle-alchemy 
  "Handles creating a potion"
  [game item]
  (let [h (engine/hero game)]
    (as-> game game
      (engine/clear-messages game)
      (engine/try-alchemy game h item)
      (end-turn game))))

(defn handle-analyse 
  "Handles analysis of a potion"
  [game item]
  (let [h (or (engine/hero game) (error "no here?!?"))]
    (as-> game game
      (engine/clear-messages game)
      (remove-thing game item) 
      (if (engine/check (? game h :IN) 5)
        (as-> game game 
          (engine/identify game item)
          (engine/message game h (str "You successfully identify " (engine/the-name game item))))
        (engine/message game h (str "You fail to identify " (engine/the-name game item))))
      (or (and (:lib game) game) (error "No game!?!?"))
      (engine/use-aps game h 100) 
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
              ;; (println (str "Turn " (:turn game) " unhandled command [" k "]")) 
              game))    
    (end-turn game)))