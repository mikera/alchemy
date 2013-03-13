(ns mikera.alchemy.lib
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util)
  (:require [mikera.alchemy.engine :as engine]))


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
;; Library-definitions - scenery

(defn define-base-scenery [lib]
  (-> lib
    (proclaim "base scenery" "base thing" 
              {:is-scenery true
               :is-view-blocking false
               :is-closed true
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

(defn define-scenery [lib]
  (-> lib
    (define-base-scenery)
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
              {:is-item true
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

(defn define-potions [lib]
  (-> lib
    
    (proclaim "potion of healing" "base potion" 
              {:colour-fg (colour 0x0090B0)})))

(defn define-ingredients [lib]
  (-> lib
    ))

(defn define-food [lib]
  (-> lib
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
                    :char \r
                    :colour-fg (colour 0xB0A090)})
    (proclaim "rat" "base rat" 
                   {:char \r
                    :colour-fg (colour 0xB0A090)})))

(defn define-hero [lib]
  (-> lib
    (proclaim "you" "base creature" 
                   {:is-hero true
                    :is-hostile false
                    :on-action nil
                    :grammatical-person :second
                    :char \@
                    :colour-fg (colour 0xFFFFFF)
                    :z-order 100})))

;; ==============================================
;; library accessors

(defn all-library-things [game]
  "Returns a list of all things possible in the game library"
  (vals (:objects (:lib game))))

(defn create
  "Creates a new thing using the library of the specified game"
  ([game name]
    (let [obj (:objects (:lib game))]
      (if-let [props (obj name)]
        (if-let [on-create (:on-create props)]
          (thing (on-create props))
          (thing props))
        (error "Can't find thing in library [" name "]" )))))

;; ==============================================
;; library main build

(defn define-objects [lib]
  (-> lib
    (define-base)
    (define-items)
    (define-tiles)
    (define-creatures)
    (define-hero)))

(defn build-lib []
  (let [lib {:objects {} ;; map of object name to properties
             }]
    (define-objects lib)))

(defn setup 
  "Sets up the object library for a given game"
  [game]
  (assoc game :lib (build-lib)))