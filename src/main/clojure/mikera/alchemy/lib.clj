(ns mikera.alchemy.lib
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util))


;; ===================================================
;; library constants

(def BLANK_TILE_PROPS {:is-tile true
                       :char \space
                       :colour-bg (colour 0x202020)
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
               :char (char 0x2593)
               :z-order 50})
    (proclaim "base floor" "base tile" 
              {:is-blocking false
               :char (char 0x00B7)
               :z-order 0})))


;; ===================================================
;; library definitions - items

(defn define-base-item [lib]
  (-> lib
    (proclaim "base item" "base thing" 
              {:is-item true
               :char (char \%)
               :z-order 20})))

(defn define-potions [lib]
  (-> lib))

(defn define-items [lib]
  (-> lib
    (define-base-item)
    (define-potions)))

;; ===================================================
;; library definitions - creatures

(defn define-creatures [lib]
  (-> lib
    (proclaim "base creature" "base thing" 
                   {:is-mobile true
                    :is-blocking true                             
                    :is-creature true
                    :z-order 75})))

(defn define-player [lib]
  (-> lib
    (proclaim "you" "base creature" 
                   {:is-player true
                    :char \@
                    :colour-fg (colour 0xFFFFFF)
                    :z-order 100})))

;; ==============================================
;; library accessors

(defn all-things [game]
  "Returns a list of all things possible in the game library"
  (vals (:objects (:lib game))))

(defn create
  "Creates a new thing using the library of the specified game"
  ([game name]
    (let [obj (:objects (:lib game))]
      (if-let [props (obj name)]
        (thing props)
        (error "Can't find thing in library [" name "]" )))))

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