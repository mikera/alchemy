(ns mikera.alchemy.engine
  (:require [mikera.cljutils.find :as find]) 
  (:use mikera.orculje.util)
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:require [mikera.orculje.engine :as en])
  (:require [mikera.alchemy.config :as config])
  (:require [mikera.orculje.mapmaker :as mm])
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

(defn hps [thing]
  (or (:hps thing) (error "No :hps avialable for " thing)))

(defn hps-max [thing]
  (or (:hps-max thing) (error "No :hps-max avialable for " thing)))

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

(defn current-level [game]
  (max 1 (min 10 (- (.z (hero-location game))))))


;; =======================================================
;; item creation
;; (these are set later by lib)

(declare create)
(declare create-type)

;; =======================================================
;; directions

(def DIRECTIONS
  {0 (loc -1 -1 0)
   1 (loc 0 -1 0)
   2 (loc 1 -1 0)
   3 (loc 1 0 0)
   4 (loc 1 1 0)
   5 (loc 0 1 0)
   6 (loc -1 1 0)
   7 (loc -1 0 0)
   8 (loc 0 0 1)
   9 (loc 0 0 -1)})

(def REVERSE-DIRECTIONS
  (into {} (map (fn [[k v]] [v k]) DIRECTIONS)))

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
                  view-ok? (or config/SEE_THROUGH_WALLS (not (get-pred game (loc px py hz) :is-view-blocking)))]
              (when view-ok?
                (.set bg px py hz true)
                (recur (+ d RAY_INC))))))))
    (extend-visibility bg) ;; extend visibility by 1 square in all directions
    (as-> game game
          (assoc game :visibility bg)
          (assoc game :discovered-world (extend-discovery bg (:discovered-world game) (:world game))))))

(defn is-square-visible? [^mikera.orculje.engine.Game game loc-or-thing                       ]
  (let [^BitGrid viz (:visibility game)
        ^mikera.orculje.engine.Location loc (if (loc? loc-or-thing) loc-or-thing (location game loc-or-thing))]
    (.get viz (.x loc) (.y loc) (.z loc))))

;; ======================================================
;; identification

(defn identify [game thing]
  (let [name (:name thing)]
    (update-in game [:lib :objects name :is-identified] (fn [old] true))))

(defn is-identified? [game thing]
  (or (:is-identified thing)
      (:is-identified ((:objects (:lib game)) (:name thing)))))

(defn identify-recipe [game thing]
  (let [name (:name thing)]
    (update-in game [:lib :objects name :is-recipe-known] (fn [old] true))))

(defn is-recipe-known? [game thing]
  (or (:is-recipe-known thing)
      (:is-recipe-known ((:objects (:lib game)) (:name thing)))))


;; ======================================================
;; action handling

(defn use-aps 
  [game thing aps-used]
  (let [aps-delta (long* (/ (* -100.0 aps-used) (or (? game thing :speed) 100)))]
    (!+ game thing :aps aps-delta)))

;; ======================================================
;; damage and healing

(def DAMAGE-TYPES {:poison {}
                   :normal {}})


(defn add-effect [game target effect]
  (add-thing game target (create game effect)))

(defn transform [game target type]
  (let [type (if (string? type) (create game type) type)]
    (merge-thing game target type)))

(defn die 
  ([game target]
    (if (not (:is-immortal target))
      (if-let [death-fn (:on-death target)]
        (death-fn game target)
        (remove-thing game target))
      game)))

(defn damage 
  ([game target amount]
    (damage game target amount :normal))
  ([game target amount type]
    (let [hps (:hps target)
          dam amount]
      (if (>= dam hps)
        (die game target)
        (! game target :hps (- hps amount))))))

(defn heal [game target amount]
  (if (:is-undead target)
    (damage game target amount)
    (let [new-hps (min (hps-max target) 
                     (+ (hps target) amount))]
      (as-> game game
            (! game target :hps new-hps)))))

;; ======================================================
;; summoning

(defn summon [game loc-or-summoner thing]
  (let [new-thing (create game thing (current-level game))
        sloc (location game loc-or-summoner)]
    (or 
      (mm/place-thing game 
                      (loc-add sloc (loc -1 -1 0))
                      (loc-add sloc (loc 1 1 0))
                      new-thing)
      (message game loc-or-summoner (str "Can't summon " (a-name game new-thing)))))) 


;; ======================================================
;; combat

(defn get-attacks [game thing]
  (let [wielded-things (filter (fn [item] 
                                 (and 
                                   (:wielded item)
                                   (:is-weapon item)
                                   (#{:left-hand :right-hand :two-hands} (:wielded item)))) 
                               (contents thing))
        atts (or (seq wielded-things) (list (:attack thing)))]
    atts))

(defn get-best-dsk [game thing]
  (let [wielded-things (filter (fn [item] 
                                 (and 
                                   (:wielded item)
                                   (:DSK item))) 
                               (contents thing))
        defs (concat (seq wielded-things) (list (:attack thing)) {:DSK 0})]
    (apply max (map #(or (:DSK %) 0)  defs))))

(defn hit [game actor target weapon critical?]
  (let [ast (* (? actor :ST) (? weapon :AST))
        hit-verb "hit"
        hit-verb (if critical? (str "skillfully " hit-verb) hit-verb)
        dam-type (or (:damage-type weapon) :normal)
        arm (+ (* 0.5 (or (? target :TG) 0)) (or (? target :ARM) 0))
        dam (* ast (Rand/nextDouble))
        dam (if critical? (+ dam (* ast (Rand/nextDouble))) dam) 
        dam (long* dam (/ dam (+ dam arm)))
        dam-str (cond 
                  (>= dam (:hps target)) 
                    (str " and " (text/verb-phrase game {:person (text/get-person actor)} "kill") " " (text/pronoun target))
                  (> dam 0) 
                    (str " for " dam " damage")
                  :else 
                    (str " but " (text/verb-phrase game {:person (text/get-person actor)} "cause") " no damage"))]
    (as-> game game
      (message game actor (str (text/verb-phrase game :the actor hit-verb :the target) dam-str "."))
      (message game target (str (text/verb-phrase game :the actor hit-verb :the target) dam-str "!"))
      (damage game target dam dam-type)
      (if-let [eff (? weapon :damage-effect)]
        (if (and (> dam 0) (< (Rand/nextDouble) (or (? weapon :damage-effect-chance) 1.0)))
          (add-effect game target eff)
          game)
        game))))
  
(defn attack 
  ([game actor target]
    (let [atts (get-attacks game actor)
          att-cost 100
          speed (or (? actor :speed) 100)]
      ;;(println (str (:name actor) " attacks with: " (vec atts)))
      (reduce
        (fn [game att] (attack game actor target att))
        (!+ game actor :aps (long* (/ (* -100.0 att-cost) speed)))
        atts)))
  ([game actor target weapon]
    (let [actor-sk (? actor :SK)
          ask (* actor-sk (? weapon :ASK))
          target-sk (? target :SK) 
          dsk (* target-sk (get-best-dsk game target))
          target-ag (? target :AG)
          dodge (* 0.25 target-ag (+ 1 (or (? target :dodge) 0)))
          critical? (check actor-sk (* 2.0 (+ target-sk target-ag)))]
      (cond 
        (not (check ask dodge))
           (as-> game game
             (message game actor (str (text/verb-phrase game :the actor "miss" :the target) "."))
             (message game target (str (text/verb-phrase game :the actor "miss" :the target) "."))) 
        (not (check ask dodge))
           (as-> game game
             (message game actor (str (text/verb-phrase game :the target "block") " your attack."))
             (message game target (str (text/verb-phrase game :the target "block") " the attack of " (the-name game actor) "."))) 
        :else 
           (hit game actor target weapon critical?)))))

;; missile attack

(defn missile-land-location [game actor tloc]
  (if (let [bl (get-blocking game tloc)] (and bl (:is-tile bl)))
      (location-towards tloc (location game actor))
      tloc))

(defn default-missile-hit [game missile actor target]
  (let []
    (as-> game game
          (message game actor (str (the-name game missile) " hits " (the-name game target)))
          (message game target (str (the-name game missile) " hits " (the-name game target)))
          (add-thing game (missile-land-location game actor (location game target)) missile))))

(defn default-throw [game missile actor target-loc]
  (let [missile-hit-fn (or (:on-missile-hit missile) default-missile-hit)
        target (get-blocking game target-loc)]
    (as-> game game
          (if (and target (not (:is-tile target))) 
            (missile-hit-fn game missile actor target)
            (add-thing game (missile-land-location game actor target-loc) missile)))))

(defn do-throw [game actor target-or-loc missile]
  (let [tloc (location game target-or-loc)
        throw-fn (or (:on-thrown missile) default-throw)] 
    (as-> game game
      (remove-thing game missile)
      (throw-fn game missile actor tloc)
      (use-aps game actor 100))))

;; ======================================================
;; actions

(defn wait [game actor]
  (use-aps game actor 100))

(defn try-open [game actor door]
  (as-> game game
    (if (? door :is-locked)
      (message game actor (str (text/verb-phrase game :the door) " is locked."))
      ((:on-open door) game door actor))    ))

(defn try-consume [game actor item]
  (as-> game game
    (if-let [con-fn (:on-consume item)]
      (con-fn game item actor)
      (message game actor (str "You don't know how to consume " (the-name game item))))    
    (use-aps game actor 100)))

(defn try-use [game actor thing]
  (as-> game game
    (if-let [on-use (? thing :on-use)]
      (on-use game thing actor)
      (message game actor (str "You have no idea what to do with " (the-name game thing) ".")))))

(defn try-attack [game thing target]
  (as-> game game
    (attack game thing target)))

(defn try-throw [game thing target-or-loc missile]
  (as-> game game
    (do-throw game thing target-or-loc missile)))

(defn try-drop [game actor item]
  (as-> game game
    (message game actor (str (text/verb-phrase game :the actor "drop" :the item) "."))
    (move-thing game item (:location actor))
    (use-aps game actor 100)))

(defn try-pickup [game actor item]
  (as-> game game
    (message game actor (str (text/verb-phrase game :the actor "take" :the item) "."))
    (move-thing game item (:id actor))
    (use-aps game actor 100)))

(defn try-bump [game thing target]
  (cond
    (:is-tile target)
      (message game thing (str (text/verb-phrase game :the thing "run") " into a wall."))
    (and (:is-being target) (is-hostile thing target))
      (try-attack game thing target)
    (and (:is-intelligent thing) (:is-door target))
      (try-open game thing target)
    :else
      (try-use game thing target)))


(defn can-move 
  "Returns true if a move or attack to a target location is possible."
  ([game m tloc]
  (let [bl (get-blocking game tloc)]
    (if (or (not bl) 
            (is-hostile m bl))
      true
      nil))))

(defn try-move
  [game thing loc]
  (if-let [target (and (not (and (:is-hero thing) config/WALK_THROUGH_WALLS (:is-tile (get-blocking game loc))))  
                       (get-blocking game loc))]
    (try-bump game thing target)
    (as-> game game
      (use-aps game thing 100)    
      (move-thing game thing loc)
      (if-let [items (and (:is-hero thing) (seq (filter :is-item (get-things game loc))))]
        (message game thing (str "There is " 
                                 (text/and-string (map (partial a-name game) items)) 
                                 " here."))
        game))))

(defn try-move-dir
  [game thing dir]
  (let [loc (location game thing)
        dir (if (check (or (:confusion thing) 0) (? thing :IN)) (DIRECTIONS (Rand/r 8)) dir)
        tloc (loc-add loc dir)]
    (try-move game thing tloc)))

;; ===================================================
;; "AI"

(defn consider-move-dir 
  "Considers whether a move is sensible. Tries it if sensible, returns nil otherwise"
  ([game m dir]
    (let [loc (location game m)
          tloc (loc-add loc dir)]
      ;;(println (str (:name m) " considering direction " dir))
      (if (can-move game m tloc)
        (try-move-dir game m dir)
        nil))))

(defn monster-action 
  "Performs one action for a monster. Assume this function only called if monster has 
   sufficient aps to make a move." 
  ([game m]
    ;;(println (str "monster thinking: " (:name m)))
    (let [m (get-thing game m)
          loc (location game m)]
      (if (is-square-visible? game loc)
        (let [hloc (hero-location game)
              dir (direction loc hloc)
              di (or (REVERSE-DIRECTIONS dir) 
                     (do 
                       ;;(println "picking random direction") 
                       (Rand/r 8)))]
          (or 
            (consider-move-dir game m dir)
            (consider-move-dir game m (DIRECTIONS (mod (inc di) 8)))
            (consider-move-dir game m (DIRECTIONS (mod (dec di) 8)))
            (consider-move-dir game m (DIRECTIONS (Rand/r 8)))
            (wait game m)))
        (if (Rand/chance 0.2)
          (try-move game m (loc-add loc (DIRECTIONS (Rand/r 8))))
          game)))))