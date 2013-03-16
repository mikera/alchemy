(ns mikera.alchemy.dungeon
  (:use mikera.orculje.core)
  (:use mikera.orculje.util)
  (:use mikera.orculje.util)
  (:import [mikera.util Rand])
  (:require [mikera.cljutils.find :as find])
  (:require [mikera.alchemy.lib :as lib])
  (:require [mikera.orculje.mapmaker :as mm]))

(defmacro and-as-> [expr sym & body]
  `(as-> ~expr ~sym
     ~@(map (fn [b] `(and ~sym ~b)) (butlast body))
     ~(last body)))

(defn maybe-place-thing [game l1 l2 t]
  (or (mm/place-thing game l1 l2 t)
      game))

(defn generate-1
  "Main dungeon generation algorithm"
  [game]
  (as-> game game
    (mm/fill-block game (loc -4 -4 0) (loc 12 4 0) (lib/create game "wall"))
    (mm/fill-block game (loc -3 -3 0) (loc 3 3 0) (lib/create game "floor"))
    (mm/fill-block game (loc 4 0 0) (loc 4 0 0) (lib/create game "floor"))
    (mm/fill-block game (loc 5 -3 0) (loc 11 3 0) (lib/create game "floor"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-apparatus]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "rat"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-reptile]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-potion]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-potion]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-potion]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "magic mushroom"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "slime mould"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-food]"))
    (maybe-place-thing game (loc 4 0 0) (loc 4 0 0) (lib/create game "door"))))


(defn generate-room [game ^mikera.orculje.engine.Location lmin 
                          ^mikera.orculje.engine.Location lmax
                          connections]
  (let [[x1 y1 z] lmin
        [x2 y2 z] lmax]
    (and-as-> game game
      (mm/fill-block game (loc (dec x1) (dec y1) z) (loc (inc x2) (inc y2) z) (lib/create game "wall"))
      (mm/fill-block game lmin lmax (lib/create game "floor"))
      (reduce (fn [game con]
                (mm/fill-block game con con (lib/create game "floor"))) game connections))))

(def TUNNEL-DIRS [(loc 1 0 0) (loc -1 0 0) (loc 0 1 0)(loc 0 -1 0)])

(defn generate-tunnel
  [game ^mikera.orculje.engine.Location lmin 
        ^mikera.orculje.engine.Location lmax
        ^mikera.orculje.engine.Location lfrom
        ^mikera.orculje.engine.Location lto]
    (if (= lfrom lto) 
      (mm/fill-block game lfrom lfrom (lib/create game "cave floor"))
      (let [game (mm/fill-block game lfrom lfrom (lib/create game "cave floor"))
            dir (if (Rand/chance 0.3)
                  (Rand/pick TUNNEL-DIRS)
                  (if (Rand/chance 0.5)
                    (loc 0 (Math/signum (double (- (.y lto) (.y lfrom)))) 0)
                    (loc (Math/signum (double (- (.x lto) (.x lfrom)))) 0 0)))
            nloc (loc-bound lmin lmax (loc-add dir lfrom))]
        (recur game lmin lmax nloc lto))))

(defn generate-caves [game ^mikera.orculje.engine.Location lmin 
                          ^mikera.orculje.engine.Location lmax
                          connections]
  (let [cloc (rand-loc lmin lmax)]
    (and-as-> game game
      (mm/fill-block game lmin lmax (lib/create game "rock wall"))
      (reduce
        (fn [game c] (generate-tunnel game lmin lmax (loc-bound lmin lmax c) cloc))
        game
        connections)
      (reduce (fn [game con]
        (mm/fill-block game con con (lib/create game "cave floor"))) game connections))))

(defn generate-block [game ^mikera.orculje.engine.Location lmin 
                          ^mikera.orculje.engine.Location lmax
                          connections]
  (or-loop [10] 
    (cond
      (Rand/chance 0.4) (generate-caves game lmin lmax connections)
      :else (generate-room game lmin lmax connections))))

(defn find-split [split-dir lmin lmax connections]
  (let [[x1 y1 z] lmin
        [x2 y2 z] lmax
        w (inc (- x2 x1))
        h (inc (- y2 y1))
        sw (if (== 0 split-dir) w h)]
    (or-loop [10]
      (let [split-point (+ 3 (Rand/r (- sw 6)))]
        (if (some (fn [^mikera.orculje.engine.Location l] (== split-point (nth l split-dir))) connections)
          nil
          split-point)))))

(def MAX_BLOCK_SIZE 20)
(def MIN_ZONE_SIZE 7)

(defn generate-zone [game ^mikera.orculje.engine.Location lmin 
                          ^mikera.orculje.engine.Location lmax
                          connections]
  ;; (println connections)
  (let [[x1 y1 z] lmin
        [x2 y2 z] lmax
        w (long (inc (- x2 x1)))
        h (long (inc (- y2 y1)))
        split-dir (if (Rand/chance 0.8) 
                    (if (> w h) 0 1)
                    (if (Rand/chance (double (/ w (+ w h)))) 0 1)) ;; prefer split on longer dimension....
        ]
    (if (or (< w MIN_ZONE_SIZE)
            (< h MIN_ZONE_SIZE)
            (and (Rand/chance 0.5) (< w MAX_BLOCK_SIZE) (< h MAX_BLOCK_SIZE)))
      (and game (generate-block game lmin lmax connections))
      (if-let [split-point (find-split split-dir lmin lmax connections)]
        (let [smax (if (== 0 split-dir) 
                   (loc (+ x1 (dec split-point)) y2 z)
                   (loc x2 (+ y1 (dec split-point)) z))
              smin (if (== 0 split-dir) 
                   (loc (+ x1 (inc split-point)) y1 z)
                   (loc x1 (+ y1 (inc split-point)) z))
              new-con (if (== split-dir 0)
                        (loc (+ x1 split-point) (+ y1 (Rand/r h)) z)
                        (loc (+ x1 (Rand/r w)) (+ y1 split-point) z))
              new-con2 (if (== split-dir 0)
                        (loc (+ x1 split-point) (+ y1 (Rand/r h)) z)
                        (loc (+ x1 (Rand/r w)) (+ y1 split-point) z))
              new-cons (if (and (> (Rand/d w) 7) (> 1 (loc-dist-manhattan new-con new-con2)))
                         [new-con new-con2]
                         [new-con])] 
          (and-as-> game game
                (or-loop [3] (generate-zone game lmin smax 
                               (concat new-cons (find/eager-filter #(loc-within? (loc-dec lmin) (loc-inc smax) %) connections))))
                (or-loop [3] (generate-zone game smin lmax 
                               (concat new-cons (find/eager-filter #(loc-within? (loc-dec smin) (loc-inc lmax) %) connections) )))))))))

(defn generate-level [game ^mikera.orculje.engine.Location lmin 
                            ^mikera.orculje.engine.Location lmax]
  (let [[x1 y1 z] lmin
        [x2 y2 z] lmax]
    (as-> game game
      (or-loop [5] 
        (generate-zone game lmin lmax [])))))

(defn generate-region [game ^mikera.orculje.engine.Location lmin 
                            ^mikera.orculje.engine.Location lmax]
  (let [[x1 y1 z1] lmin
        [x2 y2 z2] lmax]
    (and-as-> game game
          (reduce 
                 (fn [game i ] 
                    (and game (or-loop [3] 
                      (generate-level game (loc x1 y1 i) (loc x2 y2 i)))))
                  game
                  (range z1 (inc z2))))))

(defn generate 
   "Main dungeon generation algorithm"
  [game]
  (let [lmin (loc -30 -30 -10)
        lmax (loc 30 30 0)]
    (as-> game game
        (mm/fill-block game (loc-dec lmin) (loc-inc lmax) (lib/create game "rock wall"))
        (loop [game game i 10]
          (or (generate-region game lmin lmax )
              (when (> i 0)
                (println "Retrying map generation: " i)
                (recur game (dec i))))))))