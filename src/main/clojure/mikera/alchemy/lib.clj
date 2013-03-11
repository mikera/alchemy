(ns mikera.alchemy.lib
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util))

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
          object (merge obj {:name name
                             :parent-name parent-name})]
      (add-object lib object))))

;; ===================================================
;; library definitions - base

(defn define-base [lib]
  (-> lib
    (add-object 
      {:name "base object"
       :char \?
       :colour-fg (colour 0xFFFF00)
       :colour-bg (colour 0x000000)}) ;; first object in the library, has no parent
    (proclaim "base thing" "base object"
            {:id nil
             :is-thing true})))

;; ===================================================
;; library definitions - tiles

(defn define-tiles [lib]
  (-> lib
    (proclaim "base tile" "base object" {:is-tile true})
    (proclaim "base wall" "base tile" 
              {:blocking true
               :char (char 0x2593)})
    (proclaim "base floor" "base tile" 
              {:blocking false
               :char (char 0x00B7)})))


;; ===================================================
;; library definitions - items

(defn define-potions [lib]
  (-> lib))

(defn define-items [lib]
  (-> lib
    (define-potions)))

;; ===================================================
;; library definitions - creatures

(defn define-creatures [lib]
  (-> lib
    (proclaim "base creature" "base thing" 
                   {:is-mobile true
                    :is-blocking true                             
                    :is-creature true})))

(defn define-player [lib]
  (-> lib
    (proclaim "you" "base creature" 
                   {:is-player true
                    :char \@
                    :colour-fg (colour 0xFFFFFF)})))

;; ==============================================
;; library accessors

(defn all-things [game]
  "Returns a list of all things possible in the game library"
  (vals (:objects (:lib game))))

;; ==============================================
;; library main build

(defn define-objects [lib]
  (-> lib
    (define-base)
    (define-items)
    (define-tiles)
    (define-creatures)
    (define-player)))

(defn build-lib []
  (let [lib {:objects {} ;; map of object name to properties
             }]
    (define-objects lib)))

(defn setup 
  "Sets up the object library for a given game"
  [game]
  (assoc game :lib (build-lib)))