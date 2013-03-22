(ns mikera.alchemy.engine
  (:require [mikera.cljutils.find :as find]) 
  (:use mikera.orculje.util)
  (:use mikera.orculje.core)
  (:use mikera.orculje.rules)
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
  (text/num-name game thing))

(defn is-hostile [a b]
  (or (:is-hero a) (:is-hero b)))

(defn current-level [game]
  (max 1 (min 10 (- (.z (hero-location game))))))

(defn has-item? [game hero thing-or-name]
  (let [tname (if (string? thing-or-name) thing-or-name
                (:name thing-or-name))]
    (some #(= tname (:name %)) (contents hero))))

(defn remove-item [game hero thing-or-name]
  (let [tname (if (string? thing-or-name) thing-or-name
                (:name thing-or-name))
        it (find/find-first #(= tname (:name %)) (contents hero))]
    (if it
      (remove-thing game it 1)
      game)))

(defn remove-items [game hero things-or-names]
  (reduce 
    (fn [game it] (remove-item game hero it))
    game
    things-or-names))


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
    (let [^mikera.alchemy.BitGridExtender bge (mikera.alchemy.BitGridExtender. bg)]
      (.visitSetBits bg bge)
      (or (.result bge) (error "No result!!!")))))

(defn extend-discovery
  "Extends tile discovery based on current visibility"
  (^PersistentTreeGrid [^BitGrid viz ^PersistentTreeGrid discovered-world game]
    (let [^PersistentTreeGrid world (:world game)
          ^PersistentTreeGrid things (:things game)
          discovery-fn (fn [x y z]
                         (let [x (int x) y (int y) z (int z)]
                           (or 
                             (find/find-first :is-scenery (get-things game x y z))
                             (.get world x y z))))
          de (mikera.alchemy.DiscoveryExtender. viz discovered-world discovery-fn)
          ]
      (or viz (error "No visibility bitgrid?!?!"))
      (.visitSetBits viz de)
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
    (as-> game game
          (assoc game :visibility (extend-visibility bg)) ;; extend visibility by 1 square in all directions
          (assoc game :discovered-world (extend-discovery (or (:visibility game) (error "!!!!")) 
                                                          (:discovered-world game) 
                                                          game)))))

(defn is-square-visible? [^mikera.orculje.engine.Game game loc-or-thing]
  (let [^BitGrid viz (:visibility game)
        ^mikera.orculje.engine.Location loc (if (loc? loc-or-thing) loc-or-thing (location game loc-or-thing))]
    (.get viz (.x loc) (.y loc) (.z loc))))

;; =========================================================
;; messages

(defn clear-messages [game]
  (if (seq (:messages game))
    (as-> game game
          (assoc game :messages [])
          (assoc game :message-log (vec (take LOG_LENGTH (cons [] (:message-log game))))))
    game))

(defn message
  "Send a message to a given thing. Should be displayed iff it is the hero."
  ([game msg]
    (let [msg (text/capitalise msg)
          msgs (or (:messages game) [])
          new-msgs (vec (cons msg msgs))
          mlog (or (:message-log game) [msgs])
          new-mlog (assoc mlog 0 new-msgs)]
      (as-> game game
            (assoc game :messages new-msgs)
            (assoc game :message-log new-mlog))))
  ([game thing msg]
    (if (:is-hero thing)
      (message game msg) 
      game)))

(defn visible-message
  "Sends a message to the player if a given thing/location is visible"
  ([game loc-or-thing msg]
    (if (is-square-visible? game (location game loc-or-thing))
      (message game msg)
      game))) 



;; ======================================================
;; identification

(defn identify [game thing]
  (let [thing (or (get-thing game thing) thing)
        name (:name thing)]
    (update-in game [:lib :objects name :is-identified] (fn [old] true))))

(defn identify-all [game pred things]
  (reduce 
    (fn [game thing]
      (if (pred thing)
        (identify game thing)
        game))
    game 
    things))

(defn test-identified? [game thing]
  (or (:is-identified thing)
      (:is-identified ((:objects (:lib game)) (:name thing)))))

(defn is-recipe-known? [game thing]
  (or (:is-recipe-known thing)
      (:is-recipe-known ((:objects (:lib game)) (:name thing)))))

(defn identify-recipe [game thing]
  (let [name (:name thing)]
    (update-in game [:lib :objects name :is-recipe-known] (fn [old] true))))

(defn is-recipe-known? [game thing]
  (or (:is-recipe-known thing)
      (:is-recipe-known ((:objects (:lib game)) (:name thing)))))


;; ======================================================
;; action handling

(defn use-aps 
  [game actor aps-used]
  (or game (error "no game!?!?"))
  (or aps-used (error "No APS used?!?"))
  (or actor (error "no actor!?!?"))
  (let [aps-delta (long* (/ (* -100.0 aps-used) 
                            (or (? actor :speed) 100)))]
    (!+ game actor :aps aps-delta)))

;; ======================================================
;; damage and healing


(defn add-effect 
  "Apples an effect to a target. Effect may be a string or a function"
  ([game target effect]
    (if (fn? effect)
      (effect game target)
      (add-thing game target (create game effect)))))

(defn transform [game target type]
  (if-let [t (if (string? type) (create game type) type)]
    (do 
      ;;(println (str "target" (into {} target)))
      ;;(println (str "type" (into {} t)))
      (merge-thing game target t))
    (error "Can't transform into: " type)))

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
      (as-> game game 
        (! game target :hps (- hps amount))
        (if (>= dam hps)
          (die game target)
          game)))))


(defn heal [game target amount]
  (if (:is-undead target)
    (damage game target amount)
    (let [new-hps (min (hps-max target) 
                     (+ (hps target) amount))]
      (as-> game game
            (! game target :hps new-hps)))))

;; ======================================================
;; summoning

(defn summon 
  ([game loc-or-summoner thing]
    (summon game loc-or-summoner thing 1))
  ([game loc-or-summoner thing max-range]
    (let [new-thing (if (string? thing) 
                      (create game thing (current-level game))
                      thing)
          sloc (location game loc-or-summoner)]
      (or 
        (and thing (mm/place-thing game 
                        (loc-add sloc (loc (- max-range) (- max-range) 0))
                        (loc-add sloc (loc max-range max-range 0))
                        new-thing))
        (and thing (message game loc-or-summoner (str "Can't summon " (a-name game new-thing))))
        game)))) 


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
  (let [ast (double (* (? actor :ST) (? weapon :AST)))
        hit-verb (or (:hit-verb weapon) "hit")
        hit-verb (if critical? (str "skillfully " hit-verb) hit-verb)
        dam-type (or (:damage-type weapon) :normal)
        arm (calc-armour game target dam-type)
        base-dam (* ast (Rand/nextDouble))
        base-dam (max 0 (- base-dam (* arm (Rand/nextDouble))))
        base-dam (if critical? 
                   (+ base-dam (* ast (Rand/nextDouble))) 
                   base-dam) 
        dam (calc-damage game target base-dam dam-type)
        dam-str (cond 
                  (> dam 0) 
                    (str " for " dam " damage"
                         (if (>= dam (:hps target)) 
                           (str " and " (text/verb-phrase game {:person (text/get-person actor)} "kill") " " (text/pronoun target))
                           ""))
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
        (not (check ask dsk))
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
      (remove-thing game missile 1)
      (throw-fn game (merge missile {:number 1 :id nil}) actor tloc)
      (use-aps game actor 100))))

;; ======================================================
;; alchemy

(defn has-ingredients? [game hero potion]
  (let [ingreds (or (:ingredients potion) (error "potion has no ingredient list!"))]    
    (every? #(and 
               (has-item? game hero %)
               (is-identified? game (create game %))) ingreds)))


;; ======================================================
;; actions


(defn try-alchemy [game actor item]
  (as-> game game
    (message game actor (str (a-name game item) " requires ingredients: "))
    (message game actor (str "  " (text/and-string (map #(:name (create game %)) (:ingredients item)))))
    (if (has-ingredients? game actor item)
      (if (find-nearest-thing game #(= "alchemy workbench" (:name %)) actor 1)
        (let [potion (create game item)]
          (reduce
            (fn [game _] 
              (as-> game game 
                  (message game actor (str "You create " (a-name game potion) "."))
                  (remove-items game actor (:ingredients item))
                  (add-thing game actor potion)))
            game
            (range (inc (Rand/po (? actor :CR) 20)))))
        (message game actor "You need an alchemy workbench to create potions."))  
      (message game actor "Alas, you lack the necessary ingredients"))   
    (use-aps game actor 100)))

(defn wait [game actor]
  (use-aps game actor 100))

(defn try-open [game actor door]
  (as-> game game
    (use-aps game actor 100)
    (if (? door :is-locked)
      (message game actor (str (text/verb-phrase game :the door) " is locked."))
      ((:on-open door) game door actor))))

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
      (message game actor (str "You are unable to make use of " (the-name game thing) ".")))))

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
      (message game thing (str "You are blocked by " (text/verb-phrase game :the target) "."))
    (and (:is-being target) (is-hostile thing target))
      (try-attack game thing target)
    (and (:is-intelligent thing) (:is-door target))
      (try-open game thing target)
    (:is-hero thing)
      (try-use game thing target)
    :else 
      (use-aps game thing 100)))


(defn can-move-to 
  "Returns true if a move or attack to a target location is possible."
  ([game m tloc]
  (let [bl (get-blocking game tloc)]
    (if (or (not bl) 
            (and (:is-intelligent m) (:is-door bl))
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
        (message game thing (str "There " (if (== 1 (get-number (first items))) "is " "are ") 
                                 (text/and-string (map (partial a-name game) items)) 
                                 " here."))
        game))))

(defn try-exit [game hero]
  (if (has-item? game hero "The Philosopher's Stone")
    (as-> game game 
      (assoc game :game-over true)
      (message game hero "Victory! You have escaped with The Philosopher's Stone")
      (message game hero "Truly you are the greatest!"))
    (as-> game game
      (message game hero "The Philosopher's Stone is not yet in your possession.")
      (message game hero "You shall not leave without it!"))))

(defn try-move-dir
  [game thing dir]
  (let [loc (location game thing)
        dir (if (and
                  (:is-living thing)
                  (check (or (? game thing :confusion) 0) (? thing :WP))) (DIRECTIONS (Rand/r 8)) dir)
        tloc (loc-add loc dir)
        dz (dir 2)]
    (if (not (== 0 dz))
      (if-let [stairs (find-nearest-thing game :is-stairs thing 0)]
        (cond 
          (= dir (:move-dir stairs)) (try-move game thing tloc)
          (and (:is-hero thing) (= "exit staircase" (:name stairs))) (try-exit game thing)
          :else (message game thing (str "Cannot go " (if (> dz 0) "up" "down") " here")))
        (if (and (:is-hero thing) config/WALK_THROUGH_LEVELS)
          (try-move game thing tloc)
          (message game thing "There are no stairs here!")))
      (try-move game thing tloc))))

;; ===================================================
;; "AI"

(defn consider-move-dir 
  "Considers whether a move is sensible. Tries it if sensible, returns nil otherwise"
  ([game m dir]
    (let [loc (location game m)
          tloc (loc-add loc dir)]
      ;;(println (str (:name m) " considering direction " dir))
      (if (can-move-to game m tloc)
        (try-move-dir game m dir)
        nil))))

(defn make-summon-action [type & {:keys [number level max-range min-number max-number]}]
  (let [max-range (or max-range 1)
        min-number (int (or min-number number 1))
        max-number (int (or max-number min-number))]
    (fn [game m]
      (as-> game game 
            (let [level (or level (:level m))
                  number (or number (Rand/range min-number max-number))]
              (reduce 
                (fn [game _] (summon game m (create game type level) max-range))
                game
                (range number)))
            (visible-message game m (str (the-name game m) " summons help!"))
            (use-aps game m 200)))))

(defn monster-move
  "Performs a normal monster move (charge, atack, wander etc.)"
  [game m]
  (let [loc (location game m)
        tloc (:target-location m)
        hloc (hero-location game)
        same-level? (== (hloc 2) (loc 2))
        hloc (or (and same-level? (is-square-visible? game loc) hloc)
                 tloc)]
      (if hloc
        (let [dir (direction loc hloc)
              di (or (REVERSE-DIRECTIONS dir) 
                     (do 
                       ;;(println "picking random direction") 
                       (Rand/r 8)))
              game (if same-level? (! game m :target-location hloc) game)]
          (or 
            (consider-move-dir game m dir)
            (consider-move-dir game m (DIRECTIONS (mod (inc di) 8)))
            (consider-move-dir game m (DIRECTIONS (mod (dec di) 8)))
            (consider-move-dir game m (DIRECTIONS 8)) 
            (consider-move-dir game m (DIRECTIONS 9))
            (consider-move-dir game m (DIRECTIONS (Rand/r 8)))
            (as-> game game
              (! game m :target-location nil)
              (wait game m))))
        (if (Rand/chance 0.1)
          (try-move-dir game m (DIRECTIONS (Rand/r 10)))
          game)))) 

(defn monster-action 
  "Performs one action for a monster. Assume this function only called if monster has 
   sufficient aps to make a move." 
  ([game m]
    ;;(println (str "monster thinking: " (:name m)))
    (let [m (get-thing game m)
          loc (location game m)
          visible? (is-square-visible? game loc)] 
      (cond 
        (and visible? (:special-action m) (Rand/chance (double (or (:special-action-chance m) 1.0))))
          ((:special-action m) game m)
        :else (monster-move game m)))))