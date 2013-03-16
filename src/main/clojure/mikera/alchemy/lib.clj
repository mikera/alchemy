(ns mikera.alchemy.lib
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util)
  (:use mikera.orculje.text)
  (:require [mikera.alchemy.engine :as engine])
  (:require [clojure.math.combinatorics :as combo])
  (:import [mikera.util Rand]))

;; ===================================================
;; library constants

(def BLANK_TILE_PROPS {:name "nothing"
                       :proper-name "nothing"
                       :is-tile true
                       :char \space
                       :colour-fg (colour 0x000000)
                       :colour-bg (colour 0x000000)
                       :z-order (long -100)})

(def MAIN_STATS {:SK {:name "skill"}
                 :ST {:name "strength"}
                 :AG {:name "agility"}
                 :TG {:name "toughness"}
                 :IN {:name "intelligence"}
                 :WP {:name "willpower"}
                 :CH {:name "charisma"}
                 :CR {:name "craft"}})

;; ===================================================
;; utility functions for library building

(defn add-object 
  "Adds an object definition to the library. Updates any indexes necessary"
  ([lib obj]
    (assoc lib :objects 
           (assoc (:objects lib) 
                  (or (:name obj) (error "Trying to add unnamed object to lib!: " obj)) 
                  obj))))

(defn proclaim 
  "Adds an object definition to the library, deriving from an existing named object"
  ([lib name parent-name obj]
    (let [objects (:objects lib)
          parent (or
                   (objects parent-name)
                   (error "Parent not found: " parent-name))
          obj (merge parent obj)
          object (merge obj {:name name
                             :parent-name parent-name})]
      (add-object lib object))))



;; ===================================================
;; library definitions - base

(defn define-base [lib]
  (-> lib
    ;; add base object in the library, has no parent
    (add-object 
      {:name "base object"
       :char \?
       :colour-fg (colour 0xFFFF00)
       :colour-bg (colour 0x000000)
       :freq 1.0
       :level 0
       :is-visible true
       :z-order 0}) 
    (proclaim "base thing" "base object"
            {:id nil
             :is-thing true})))

;; ===================================================
;; library definitions - tiles

(defn define-tiles [lib]
  (-> lib
    (proclaim "base tile" "base object" 
              {:is-tile true
               :z-order 25})
    (proclaim "base wall" "base tile" 
              {:is-blocking true
               :is-view-blocking true
               :colour-fg (colour 0x808080) 
               :colour-bg (colour 0x808000) 
               :char (char 0x2591)
               :z-order 50})
    (proclaim "base floor" "base tile" 
              {:is-blocking false
               :char (char 0x00B7)
               :z-order 0})
    (proclaim "floor" "base floor" 
              {:colour-fg (colour 0x404040)})
    (proclaim "wall" "base wall" 
              {:colour-fg (colour 0xC08040)
               :colour-bg (colour 0x804000)})))

;; ===================================================
;; Library definitions - effects

(defn define-base-effects [lib]
  (-> lib
    (proclaim "base effect" "base thing" 
              {:is-effect true
               :is-invisible true
               :char (char \#)
               :z-order -1})
    (proclaim "base temporary effect" "base effect" 
              {:on-action (fn [game effect]
                            (let [elapsed (:aps effect)
                                  lifetime (- (:lifetime effect) elapsed)]
                              ;; (println "effect time elapsed = " elapsed " remainging lifetime = " lifetime)
                              (if (<= lifetime 0)
                                (remove-thing game effect)
                                (update-thing game (-> effect
                                                     (assoc :aps 0)
                                                     (assoc :lifetime lifetime)))))) 
               :aps 0     
               :lifetime 3000})
    (proclaim "base periodic effect" "base effect" 
              {:on-action (fn [game effect]
                            (let [elapsed (:aps effect)
                                  prev-lifetime (:lifetime effect)
                                  lifetime (- prev-lifetime elapsed)
                                  period (:period effect)
                                  target (parent game effect)
                                  periods (- (quot prev-lifetime period) (quot lifetime period))]
                              ;; (println "effect time elapsed = " elapsed " remainging lifetime = " lifetime)
                              (as-> game game
                                (loop [i periods game game]
                                  (if (> i 0)
                                    (recur (dec i) ((:on-effect effect) game effect target))
                                    game))
                                (if (<= lifetime 0)
                                  (remove-thing game effect)
                                  (update-thing game (-> effect
                                                       (assoc :aps 0)
                                                       (assoc :lifetime lifetime))))))) 
               :on-effect (fn [game effect target]
                            game)
               :aps 0     
               :period 300
               :lifetime 3000})))


(defn define-periodic-effects [lib]
  (-> lib
    (proclaim "healing" "base periodic effect"
              {:lifetime 1000
               :period 100
               :heal-amount 1
               :parent-modifiers [(modifier :colour-bg (colour 0x004040))]
               :on-effect (fn [game effect target]
                            (engine/heal game target (:heal-amount effect)))}
              )
    (proclaim "regenerating" "base periodic effect"
              {:lifetime 20000
               :period 500
               :heal-amount 1
               :parent-modifiers [(modifier :colour-bg (colour 0x006020))]
               :on-effect (fn [game effect target]
                            (engine/heal game target (:heal-amount effect)))}
              )
    (proclaim "poisoned" "base periodic effect"
              {:lifetime 4000
               :period 400
               :is-poison-effect true
               :damage-amount 1
               :parent-modifiers [(modifier :colour-bg (colour 0x008000))]
               :on-effect (fn [game effect target]
                            (engine/damage game target (:damage-amount effect) :poison))}
              )
    (proclaim "poisoned!" "poisoned" {:damage-amount 2 :lifetime 6000 :period 400})
    (proclaim "poisoned!!" "poisoned" {:damage-amount 3 :lifetime 6000 :period 300})
    (proclaim "poisoned!!!" "poisoned" {:damage-amount 3 :lifetime 6000 :period 200})
    (proclaim "poisoned!!!!" "poisoned" {:damage-amount 3 :lifetime 6000 :period 100})    
    (proclaim "sick" "base periodic effect"
              {:lifetime 100000
               :period 10000
               :damage-amount 2
               :parent-modifiers (vec (concat [(modifier :colour-bg (colour 0x400040))]
                                              (map #(modifier % (long* value 0.8)) 
                                                   [:SK :ST :AG :TG :IN :WP :CH :CR])))
               :on-effect (fn [game effect target]
                            (engine/damage game target (:damage-amount effect) :poison))}
              )))

(defn define-temp-effects [lib]
  (-> lib
    (proclaim "invincible" "base temporary effect"
              {:lifetime 2000
               :parent-modifiers [(modifier :colour-fg (colour (Rand/r 0x1000000)))
                                  (modifier :ARM (+ value 100))]
               })
    (proclaim "shielded" "base temporary effect"
              {:lifetime 5000
               :parent-modifiers [(modifier :colour-fg (colour (Rand/r 0x90FFFF)))
                                  (modifier :ARM (+ value 5))]
               })
    (proclaim "slowed" "base temporary effect"
              {:lifetime 2000
               :parent-modifiers [(modifier :speed (long* value 0.7))]})  
    (proclaim "slowed!" "base temporary effect"
              {:lifetime 3000
               :parent-modifiers [(modifier :speed (long* value 0.5))]})  
    (proclaim "hasted" "base temporary effect"
              {:lifetime 2000
               :parent-modifiers [(modifier :speed (+ value 50))]})  
    (proclaim "hasted!" "base temporary effect"
              {:lifetime 1500
               :parent-modifiers [(modifier :speed (+ value 100))]})  
    (proclaim "weakened" "base temporary effect"
              {:lifetime 3000
               :parent-modifiers [(modifier :ST (long* value 0.7))
                                  (modifier :TG (long* value 0.7))]})  
    (proclaim "weakened!" "base temporary effect"
              {:lifetime 4000
               :parent-modifiers [(modifier :ST (long* value 0.5))
                                  (modifier :TG (long* value 0.5))
                                  (modifier :AG (long* value 0.7))]})  
    (proclaim "confused" "base temporary effect"
              {:lifetime 20000
               :parent-modifiers [(modifier :confusion (+ value 3))]})       
    (proclaim "confused!" "base temporary effect"
              {:lifetime 10000
               :parent-modifiers [(modifier :confusion (+ value 8))]})
    (proclaim "confused!!" "base temporary effect"
              {:lifetime 5000
               :parent-modifiers [(modifier :confusion (+ value 20))]})))

(defn proclaim-stat-effects 
  ([lib]
    (reduce (fn [lib stat] (proclaim-stat-effects lib stat)) lib (keys MAIN_STATS)))
  ([lib stat]
    (let [BOOST_EFFECT (Rand/d 5)
          statname (or (:name (MAIN_STATS stat)) (error "No name for stat " stat))]
      (-> lib 
        (proclaim (str statname " boost") "base temporary effect"
                  {:level (Rand/d 20)
                   :lifetime (+ 3000 (* 1000 (Rand/r 5)))
                   :parent-modifiers [(modifier stat (+ value BOOST_EFFECT))]}
                  )
        (proclaim (str statname " drain") "base temporary effect"
                  {:level (Rand/d 10)
                   :lifetime (+ 5000 (* 2000 (Rand/r 5)))
                   :parent-modifiers [(modifier stat (long* value 0.8))]}
                  )))))

(defn define-effects [lib]
  (-> lib
    (define-base-effects)
    (define-periodic-effects)
    (define-temp-effects)
    (proclaim-stat-effects)))



;; ===================================================
;; Library-definitions - scenery

(defn define-base-scenery [lib]
  (-> lib
    (proclaim "base scenery" "base thing" 
              {:is-scenery true
               :is-view-blocking false
               :is-blocking true
               :char (char \#)
               :hps 30
               :z-order 60})))

(defn define-doors [lib]
  (-> lib
    (proclaim "base door" "base scenery" 
              {:is-door true
               :closed-properties {:char (char 0x256C)                             
                                   :is-open false
                                   :colour-fg (colour 0x000000) 
                                   :colour-bg (colour 0xC07020)
                                   :is-blocking true
                                   :z-order 60
                                   :is-view-blocking true} 
               :open-properties {:char (char 0x2551)
                                 :is-open true
                                 :is-locked false
                                 :colour-bg (colour 0x000000) 
                                 :colour-fg (colour 0xC07020)
                                 :is-blocking false
                                 :z-order 1
                                 :is-view-blocking false
                                 }
               :on-open (fn [game door actor]
                          (update-thing game 
                                        (merge door
                                               (if (? door :is-open)
                                                 (? door :closed-properties)
                                                 (? door :open-properties))))) 
               :on-create (fn [door]
                            (merge door ((if (:is-open door) :open-properties :closed-properties) door)))
               :z-order 70})
    (proclaim "door" "base door" 
              {})
    (proclaim "grille" "base door" 
              {:is-locked true
               :closed-properties {:char \#                            
                                   :is-open false
                                   :is-locked true
                                   :colour-fg (colour 0x909090) 
                                   :colour-bg (colour 0x000000)
                                   :is-blocking true
                                   :is-view-blocking false} })))



(defn define-apparatus [lib]
  (-> lib
    (proclaim "base apparatus" "base scenery" 
              {:is-apparatus true
               :on-use (fn [game app actor]
                         (engine/message game actor (str "You don't know how to use " (the-name game app) ".")))})
    (proclaim "alchemy bench" "base apparatus" 
              {:char (char 0x046C)})))

(defn define-stairs [lib]
  (-> lib
    (proclaim "base stairs" "base scenery" 
              {:is-stairs true
               :is-blocking false
               :is-immortal true
               :on-use (fn [game app actor]
                         (engine/message game actor (str "You don't know how to use " (the-name game app) ".")))})
    (proclaim "up staircase" "base stairs" 
              {:move-dir (loc 0 0 -1)
               :char \<})
    (proclaim "down staircase" "base stairs" 
              {:move-dir (loc 0 0 1)
               :char \>})))

(defn define-scenery [lib]
  (-> lib
    (define-base-scenery)
    (define-apparatus)
    (define-stairs)
    (define-doors)))

;; ===================================================
;; library definitions - ITEMS


(defmacro consume-function [[game item actor :as binds] & body]
  (when-not (and (vector? binds) (every? symbol? binds)) (error "consume function requires [game actor item] bindings")) 
  `(fn [~game ~item ~actor ]
     (as-> ~game ~game
       (remove-thing ~game ~item)
       ~@body
       (engine/identify ~game ~item)
       )))


(defn potion-effect-function [effect-name]
  (fn [game potion target]
    (as-> game game
          (remove-thing game potion)
          (engine/add-effect game target effect-name)
          (engine/identify game potion)
)))


(defn define-base-item [lib]
  (-> lib
    (proclaim "base item" "base thing" 
              {:is-item true
               :char (char \%)
               :z-order 20})
    (proclaim "base ingredient" "base item" 
              {:is-ingredient true
               :is-item true
               :char (char 0x2663)
               :colour-fg (colour 0x008000) 
               :z-order 20})
    (proclaim "base potion" "base item" 
              {:is-potion true
               :char (char \!)
               :on-consume  (consume-function [game item actor]
                              (engine/message game actor "Yuck. This potions tastes terrible.")
                              (do (println (str "Potion has no consume defined: " (:name item))) game))
               :on-splash (consume-function [game item target]
                            (engine/message game target (str "You are splashed with " (the-name game item) "!"))
                            ((:on-consume item) game item target))
               :colour-fg (colour 0x0090B0) 
               :z-order 25})
    (proclaim "base food" "base ingredient" 
              {:is-food true
               :is-ingredient false
               :char (char 0x2663)
               :food-value 100
               :on-consume  (consume-function [game item actor]
                              (!+ actor :food (:food-value item))
                              (engine/message game actor (str "Mmmm. " (engine/a-name game item) " .Tasty.")))
               :colour-fg (colour 0xC00000) 
               :z-order 20})))


(def POTION-COLOURS {"blue" {:colour-fg (colour 0x0000FF)}
                     "red" {:colour-fg (colour 0xFF0000)}
                     "green" {:colour-fg (colour 0x00FF00)}
                     "orange" {:colour-fg (colour 0xFF8000)}
                     "pink" {:colour-fg (colour 0xFFA0A0)}
                     "aquamarine" {:colour-fg (colour 0x00FFFF)}
                     "white" {:colour-fg (colour 0xFFFFFF)}
                     "black" {:colour-fg (colour 0x404040)}
                     "grey" {:colour-fg (colour 0x909090)}
                     "purple" {:colour-fg (colour 0xFF00A0)}
                     "indigo" {:colour-fg (colour 0xA000FF)}
                     "lime green" {:colour-fg (colour 0x70FF90)}
                     "brown" {:colour-fg (colour 0x806030)}
                     "yellow" {:colour-fg (colour 0xFFFF00)}})

(def POTION-ADJECTIVES ["bubbling" "fizzy" "milky" "oily" "shining" "glowing"
                         "pulsating" "turbulent" "frothy" "metallic" "transparent"
                         "viscous" "glittering" "smelly" "noxious" "fragrant"])

(defn describe-potions [lib]
  (let [all-potions (filter #(and (:is-potion %) (> (:freq %) 0)) (vals (:objects lib)))
        combos (shuffle (combo/cartesian-product POTION-ADJECTIVES (keys POTION-COLOURS)))
        potions (map 
                  (fn [p [a c]]
                    (-> p
                      (merge (POTION-COLOURS c))
                      (assoc :unidentified-name (str a " " c " potion"))))
                  all-potions
                  combos)
        updated-potion-map (zipmap (map :name potions) potions)]
    (assoc lib :objects (merge (:objects lib) updated-potion-map))))

(defn proclaim-stat-potions 
  ([lib]
    (reduce (fn [lib stat] (proclaim-stat-potions lib stat)) lib (keys MAIN_STATS)))
  ([lib stat]
    (let [statname (:name (MAIN_STATS stat))]
      (-> lib
        (proclaim (str "gain " statname " potion") "base potion" 
              {:level (+ 5 (Rand/d 15))
               :on-consume  (consume-function [game item actor]
                              (!+ actor stat 1) (engine/message game actor (str "You feel you have gained in " statname "!")))})
        (proclaim (str statname " boost potion") "base potion" 
              {:level (Rand/d 15)
               :on-consume (potion-effect-function (str statname " boost"))})))))

(defn define-potions [lib]
  (-> lib
    (proclaim-stat-potions)
    (proclaim "health boost potion" "base potion" 
              {:level 0 
               :on-consume (consume-function [game item actor]
                             (let [hps (:hps actor)
                                   new-hps (+ hps (Rand/r (:TG actor)))]
                               (! actor :hps new-hps)
                               (engine/message game actor
                                 (if (> new-hps hps) "You feel healthier." "You feel very healthy."))))})
    (proclaim "cure poison potion" "base potion" 
              {:level 0 
               :on-consume (consume-function [game item actor]
                             (if-let [ps (seq (filter :is-poison-effect (contents actor)))]
                               (as-> game game 
                                     (reduce (fn [game t] (remove-thing game t)) game ps)
                                     (engine/message game actor "You feel much better!"))
                               (engine/message game actor "Hmmm that was quite refreshing.")))})
    (proclaim "cleansing potion" "base potion" 
              {:level 7 
               :on-consume (consume-function [game item actor]
                             (if-let [ps (seq (filter :is-effect (contents actor)))]
                               (as-> game game 
                                     (reduce (fn [game t] (remove-thing game t)) game ps)
                                     (engine/message game actor "You feel great refreshment!"))
                               (engine/message game actor "You feel totally refreshed.")))})
    (proclaim "healing potion" "base potion" 
              {:level 0
               :on-consume (potion-effect-function "healing")})
    (proclaim "poison potion" "base potion" 
              {:level 1
               :on-consume (potion-effect-function "poisoned")})
    (proclaim "confusion potion" "base potion" 
              {:level 1
               :on-consume (potion-effect-function "confused")})
    (proclaim "sickness potion" "base potion" 
              {:level 5
               :on-consume (potion-effect-function "sick")})
    (proclaim "regeneration potion" "base potion" 
              {:level 8
               :on-consume (potion-effect-function "regenerating")})
    (proclaim "weakness potion" "base potion" 
              {:level 0
               :on-consume (potion-effect-function "weakened")})
    (proclaim "confusion potion" "base potion" 
              {:level 0
               :on-consume (potion-effect-function "confused")})
    (proclaim "slow potion" "base potion" 
              {:level (Rand/d 10) 
               :on-consume (potion-effect-function "slowed")})
    (proclaim "extreme slow potion" "base potion" 
              {:level (Rand/d 20) 
               :on-consume (potion-effect-function "slowed!")})
    (proclaim "speed potion" "base potion" 
              {:level (Rand/d 10) 
               :on-consume (potion-effect-function "hasted")})
    (proclaim "extreme speed potion" "base potion" 
              {:level (Rand/d 20) 
               :on-consume (potion-effect-function "hasted!")})
    (proclaim "greater confusion potion" "base potion" 
              {:level (Rand/d 10) 
               :on-consume (potion-effect-function "confused!")})
    (proclaim "extreme confusion potion" "base potion" 
              {:level (Rand/d 20) 
               :on-consume (potion-effect-function "confused!!")})
    (proclaim "shielding potion" "base potion" 
              {:level 3
               :on-consume (potion-effect-function "shielded")})
    (describe-potions)))

(defn define-herbs [lib]
  (-> lib
    (proclaim "base herb" "base ingredient" 
              {:char (char 0x1D67)
               :colour-fg (colour 0x00B030) 
               :unidentified-name "strange herb" 
               :food-value 1})
    (proclaim "fairgrass weed" "base herb" 
              {})
    (proclaim "ironroot herb" "base herb" 
              {})
    (proclaim "copperleaf herb" "base herb" 
              {})
    (proclaim "wolfsbane herb" "base herb" 
              {})
    (proclaim "limegrass herb" "base herb" 
              {})))

(defn define-ingredients [lib]
  (-> lib
    (define-herbs)))

(defn define-food [lib]
  (-> lib
    (proclaim "base mushroom" "base food" 
              {:char (char 0x2660)
               :food-value 10})
    (proclaim "magic mushroom" "base mushroom" 
              {:level 1
               :is-ingredient true
               :char (char 0x2660)
               :food-value 10
               :modifiers {:colour-fg 
                             [(modifier :colour-fg (colour (Rand/r 0x1000000)))]}})
    (proclaim "slime mould" "base food" 
              {:level 3
               :food-value 100})))

;; ===================================================
;; libary definitions - weapons & attacks

(def WIELD-TYPES {:right-hand {:replaces [:two-hands]}
                   :left-hand {:replaces [:two-hands]}
                   :two-hands {:replaces [:right-hand :left-hand]}
                   :head {:replaces [:two-hands]}
                   :body {:replaces [:two-hands]}
                   :legs {:replaces [:two-hands]}
                   :full-body {:replaces [:body]}
                   :cloak {} 
                   :necklace {} 
                   :right-ring {}
                   :left-ring {}
                   :feet {}})

(def ATT_NORMAL {:name "normal attack" 
                 :ASK 1.0 :DSK 0.75 :AST 1.0 
                 :damage-type :normal})

(def ATT_BITE {:name "bite attack" 
                 :ASK 1.0 :DSK 0.0 :AST 1.0   ;; no dsk - can't block with a bite!
                 :damage-type :normal})

(def ATT_POISON_BITE {:name "poison bite" 
                 :ASK 1.0 :DSK 0.0 :AST 0.75 
                 :damage-type :normal
                 :damage-effect "poisoned"})

(def ATT_SWORD {:name "sword" 
                :ASK 1.2 :DSK 1.0 :AST 1.2 
                :damage-type :normal 
                :wield-types [:right-hand :left-hand]})
(def ATT_AXE {:name "axe" 
              :ASK 1.0 :DSK 0.5 :AST 1.5 
              :damage-type :normal 
              :wield-types [:right-hand :left-hand]})
(def ATT_MACE {:name "mace" 
               :ASK 1.0 :DSK 0.5 :AST 1.5 
               :damage-type :impact 
               :wield-types [:right-hand :left-hand]})
(def ATT_DAGGER {:name "dagger"
                 :ASK 1.2 :DSK 0.8 :AST 0.8 
                 :damage-type :normal 
                 :wield-types [:right-hand :left-hand]})

(defn define-weapons [lib]
  (-> lib ))


;; ===================================================
;; main item library definitions
(defn define-items [lib]
  (-> lib
    (define-base-item)
    (define-scenery)
    (define-ingredients)
    (define-food)
    (define-potions)
    (define-weapons)))

;; ===================================================
;; library definitions - creatures

(defn define-creatures [lib]
  (-> lib
    (proclaim "base creature" "base thing" 
                   {:is-mobile true
                    :is-blocking true                             
                    :is-creature true
                    :is-hostile true
                    :drop-chance 1.0
                    :drop-type "[:is-item]"
                    :on-action engine/monster-action
                    :on-death (fn [game thing]
                                (if-let [l (location game thing)]
                                  (as-> game game
                                    (if-let [dtype (and (> (or (:drop-chance thing) 0) (Rand/nextDouble)) (:drop-type thing))]
                                      (add-thing game l (engine/create game dtype (or (:level thing) 1)))
                                      game)
                                    (remove-thing game thing))
                                  game))
                    :attack ATT_NORMAL
                    :speed 100
                    :aps 0
                    :z-order 75
                    :SK 5 :ST 5 :AG 5 :TG 5 :IN 5 :WP 5 :CH 5 :CR 5})
    (proclaim "base rat" "base creature" 
                   {:SK 4 :ST 3 :AG 6 :TG 2 :IN 1 :WP 5 :CH 2 :CR 2
                    :attack ATT_BITE
                    :hps 3
                    :char \r
                    :colour-fg (colour 0xB0A090)})
    (proclaim "rat" "base rat" 
                   {:level 0})
    
    (proclaim "base snake" "base creature" 
                   {:SK 5 :ST 3 :AG 8 :TG 4 :IN 2 :WP 6 :CH 4 :CR 1
                    :is-reptile true
                    :attack ATT_BITE
                    :hps 3
                    :char \s
                    :level 1
                    :colour-fg (colour 0x60C060)})
    (proclaim "grass snake" "base snake"
                   {})
    (proclaim "cobra" "base snake"
                   {:SK 9 :ST 6 :AG 10 :TG 7 :IN 4 :WP 9 :CH 8 :CR 3
                    :char \c
                    :hps 15
                    :level-min 6
                    :attack ATT_POISON_BITE
                    :colour-fg (colour 0xD0A060)})))

(defn define-hero [lib]
  (-> lib
    (proclaim "you" "base creature" 
                   {:is-hero true
                    :is-intelligent true
                    :is-hostile false
                    :on-action nil
                    :on-death (fn [game hero]
                                (as-> game game
                                      (engine/message game hero "You have died....") 
                                      (engine/message game hero "[Press R to restart]") 
                                      (! hero :char \%)
                                      (! hero :is-corpse true)
                                      (assoc game :game-over true)))
                    :hps 15
                    :attack ATT_NORMAL
                    :grammatical-person :second
                    :char \@
                    :colour-fg (colour 0xFFFFFF)
                    :z-order 100})))

;; ==============================================
;; library accessors

(defn all-library-things [game]
  "Returns a list of all things possible in the game library"
  (vals (:objects (:lib game))))

(defn create-type 
  ([game pred]
    (create-type game pred (:max-level game)))
  ([game pred level]
    (let [objs (seq (vals (:objects (:lib game))))]
      (loop [v nil cumfreq 0.0 objs objs]
        (if objs
          (let [o (first objs)
                valid? (and (pred o) (>= level (or (:level-min o) 0)))
                freq-o (if (pred o) (double (:freq o)) 0.0)
                keeper? (< (* (Rand/nextDouble) (+ cumfreq freq-o)) freq-o)]
            (recur (if keeper? o v) (+ cumfreq freq-o) (next objs)))
          (thing v))))))

(defn create
  "Creates a new thing using the library of the specified game"
  ([game ^String name]
    (create game name (or (:max-level name) 0)))
  ([game ^String name level]
    (let [obj (:objects (:lib game))]
      (if-let [props (obj name)]
        (if-let [on-create (:on-create props)]
          (thing (on-create props))
          (thing props))
        (cond 
          (.startsWith name "[")
            (create-type game (keyword (.substring name 2 (dec (count name)))) level)
          :else
            (error "Can't find thing in library [" name "]" ))))))

(alter-var-root (var engine/create) (fn [old] create) )
(alter-var-root (var engine/create-type) (fn [old] create-type))

;; ==============================================
;; library main build

(defn define-objects [lib]
  (-> lib
    (define-base)
    (define-tiles)
    (define-effects)
    (define-items)
    (define-creatures)
    (define-hero)))

(defn post-process-properties [v]
  (as-> v v
    (if (.startsWith (:name v) "base ") (assoc v :freq 0.0) v)
    (assoc v :level (or (:level v) 0))
    (assoc v :level-min (or (:level-min v) (:level v) 1))
    (if (and (:hps v) (not (:hps-max v))) (assoc v :hps-max (:hps v)) v)))

(defn post-process 
  ([objects]
    (into {}
          (map (fn [[k v]]
                 [k (post-process-properties v)])
               objects))))

(defn assign-potion-ingredients 
  "Assigns 1-3 ingredients to every potion"
  ([objects]
    (let [obs (vals objects)
          ingreds (vec (map :name (filter :is-ingredient (vals objects))))
          icount (count ingreds)
          potions (filter :is-potion (vals objects))
          rand-ingredients (fn []
                             (vec (distinct 
                                    (for [i (range (inc (Rand/d 2) ))]
                                      (ingreds (Rand/r icount))))))
          new-potions (map (fn [o] (assoc o :ingredients (rand-ingredients))) potions)]
      (reduce (fn [obs o] (assoc obs (:name o) o)) objects new-potions))))

(defn build-lib []
  (as-> {:objects {} ;; map of object name to properties
         } 
        lib 
    (define-objects lib)
    (assoc lib :objects (post-process (:objects lib)))
    (assoc lib :objects (assign-potion-ingredients (:objects lib)))))

(defn setup 
  "Sets up the object library for a given game"
  [game]
  (assoc game :lib (build-lib)))