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

(def BLANK_TILE_PROPS {:is-tile true
                       :char \space
                       :colour-fg (colour 0x000000)
                       :colour-bg (colour 0x000000)
                       :z-order (long -100)})

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
    (proclaim "base temporary effect" "base thing" 
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
               :lifetime 3000})))

(defn define-temp-effects [lib]
  (-> lib
    (proclaim "invincibility" "base temporary effect"
              {:lifetime 2000
               :parent-modifiers [(modifier :colour-fg (colour (Rand/r 0x1000000)))
                                  (modifier :ARM (+ value 100))]
               }
              )))

(defn define-effects [lib]
  (-> lib
    (define-base-effects)
    (define-temp-effects)))

;; ===================================================
;; Library-definitions - scenery

(defn define-base-scenery [lib]
  (-> lib
    (proclaim "base scenery" "base thing" 
              {:is-scenery true
               :is-view-blocking false
               :is-blocking true
               :char (char \#)
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
                                   :is-view-blocking true} 
               :open-properties {:char (char 0x2551)
                                 :is-open true
                                 :is-locked false
                                 :colour-bg (colour 0x000000) 
                                 :colour-fg (colour 0xC07020)
                                 :is-blocking false
                                 :is-view-blocking false
                                 }
               :on-open (fn [game door actor]
                          (update-thing game (merge door
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

(defn define-scenery [lib]
  (-> lib
    (define-base-scenery)
    (define-apparatus)
    (define-doors)))

;; ===================================================
;; library definitions - items

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
               :colour-fg (colour 0x0090B0) 
               :z-order 25})
    (proclaim "base food" "base ingredient" 
              {:is-food true
               :char (char 0x2663)
               :food-value 100
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

(defn define-potions [lib]
  (-> lib
    (proclaim "strengthening potion" "base potion" 
              {:colour-fg (colour 0x0090B0)})
    (proclaim "power potion" "base potion" 
              {:colour-fg (colour 0x0090B0)})
    (proclaim "healing potion" "base potion" 
              {:colour-fg (colour 0x0090B0)})
    (describe-potions)))

(defn define-ingredients [lib]
  (-> lib
    ))

(defn define-food [lib]
  (-> lib
    (proclaim "base mushroom" "base ingredient" 
              {:char (char 0x2660)
               :food-value 10})
    (proclaim "magic mushroom" "base mushroom" 
              {:char (char 0x2660)
               :food-value 10
               :modifiers {:colour-fg 
                             [(modifier :colour-fg (colour (Rand/r 0x1000000)))]}})
    (proclaim "slime mould" "base food" 
              {:food-value 100})))

(defn define-items [lib]
  (-> lib
    (define-base-item)
    (define-scenery)
    (define-ingredients)
    (define-food)
    (define-potions)))

;; ===================================================
;; library definitions - creatures

(defn define-creatures [lib]
  (-> lib
    (proclaim "base creature" "base thing" 
                   {:is-mobile true
                    :is-blocking true                             
                    :is-creature true
                    :is-hostile true
                    :on-action engine/monster-action
                    :aps 0
                    :z-order 75
                    :SK 5 :ST 5 :AG 5 :TG 5 :IN 5 :WP 5 :CH 5 :CR 5})
    (proclaim "base rat" "base creature" 
                   {:SK 4 :ST 3 :AG 6 :TG 2 :IN 1 :WP 5 :CH 2 :CR 2
                    :hps 3
                    :char \r
                    :colour-fg (colour 0xB0A090)})
    (proclaim "rat" "base rat" 
                   {:level-min 1})
    (proclaim "base snake" "base creature" 
                   {:SK 5 :ST 3 :AG 8 :TG 4 :IN 2 :WP 6 :CH 4 :CR 1
                    :is-reptile true
                    :hps 3
                    :char \s
                    :level-min 1
                    :colour-fg (colour 0x60C060)})
    (proclaim "grass snake" "base snake"
                   {})
    (proclaim "cobra" "base snake"
                   {:SK 9 :ST 6 :AG 10 :TG 7 :IN 4 :WP 9 :CH 8 :CR 3
                    :char \c
                    :hps 15
                    :level-min 4
                    :colour-fg (colour 0xD0A060)})))

(defn define-hero [lib]
  (-> lib
    (proclaim "you" "base creature" 
                   {:is-hero true
                    :is-hostile false
                    :on-action nil
                    :hps 15
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
  [game pred]
  (let [objs (seq (vals (:objects (:lib game))))]
    (loop [v nil cumfreq 0.0 objs objs]
      (if objs
        (let [o (first objs)
              freq-o (if (pred o) (double (:freq o)) 0.0)
              keeper? (< (* (Rand/nextDouble) (+ cumfreq freq-o)) freq-o)]
          (recur (if keeper? o v) (+ cumfreq freq-o) (next objs)))
        (thing v)))))

(defn create
  "Creates a new thing using the library of the specified game"
  ([game ^String name]
    (let [obj (:objects (:lib game))]
      (if-let [props (obj name)]
        (if-let [on-create (:on-create props)]
          (thing (on-create props))
          (thing props))
        (cond 
          (.startsWith name "[")
            (create-type game (keyword (.substring name 2 (dec (count name)))))
          :else
            (error "Can't find thing in library [" name "]" ))))))

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
    (if (and (:hps v) (not (:hps-max v))) (assoc v :hps-max (:hps v)) v)))

(defn post-process 
  ([objects]
    (into {}
          (map (fn [[k v]]
                 [k (post-process-properties v)])
               objects))))

(defn build-lib []
  (as-> {:objects {} ;; map of object name to properties
         } 
        lib 
    (define-objects lib)
    (assoc lib :objects (post-process (:objects lib)))))

(defn setup 
  "Sets up the object library for a given game"
  [game]
  (assoc game :lib (build-lib)))