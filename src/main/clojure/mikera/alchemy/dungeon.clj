(ns mikera.alchemy.dungeon
  (:use mikera.orculje.core)
  (:use mikera.orculje.util)
  (:use mikera.cljutils.error)
  (:import [mikera.util Rand])
  (:require [mikera.cljutils.find :as find])
  (:require [mikera.alchemy.lib :as lib])
  (:require [mikera.orculje.mapmaker :as mm]))

(defmacro and-as-> [expr sym & body]
  `(as-> ~expr ~sym
     ~@(map (fn [b] `(and ~sym ~b)) (butlast body))
     ~(last body)))

(defn maybe-place-thing 
  ([game l1 l2 t]
    (or (and t (mm/place-thing game l1 l2 t))
        game)))

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

(defn ensure-door 
  ([game loc]
    (if (seq (get-things game loc))
      game
      (maybe-place-thing game loc loc (lib/create game "[:is-door]")))))

(defn ensure-doors [game room]
  (reduce 
    ensure-door
    game
    (:connections room)))

(defn decorate-lair [game room]
  (let [lmin (:lmin room)
        lmax (:lmax room)]
    (let [mtype (Rand/pick ["[:is-undead]" "[:is-creature]" "[:is-goblinoid]" "[:is-snake]"])]
      (reduce 
        (fn [game _]
          (and-as-> game game
                    (maybe-place-thing game lmin lmax (lib/create game mtype (- (lmin 2))))
                    (maybe-place-thing game lmin lmax (lib/create game "[:is-item]" (- (lmin 2))))))
        game
        (range (Rand/d 10))))))

(defn decorate-store-room [game room type]
  (let [lmin (:lmin room)
        lmax (:lmax room)]
    (reduce 
      (fn [game _]
        (and-as-> game game
          (maybe-place-thing game lmin lmax (lib/create game type))))
      game
      (range (Rand/d 12)))))

(defn decorate-lab [game room]
  (let [lmin (:lmin room)
        lmax (:lmax room)]
    (and-as-> game game
      (reduce 
        (fn [game _]
          (maybe-place-thing game lmin lmax (lib/create game "[:is-potion]")))
        game
        (range (Rand/d 4)))
      (maybe-place-thing game lmin lmax (lib/create game "[:is-apparatus]")))))

(defn decorate-normal-room [game room]
  (let [lmin (:lmin room)
        lmax (:lmax room)]
    (and-as-> game game
      (if (Rand/chance 0.3) (maybe-place-thing game lmin lmax (lib/create game "[:is-creature]" (- (lmin 2)))) game)        
      (if (Rand/chance 0.5) (maybe-place-thing game lmin lmax (lib/create game "[:is-potion]" (- (lmin 2)))) game)
      )))

(defn decorate-designer-room [game room]
  (let [lmin (:lmin room)
        lmax (:lmax room)
        [x1 y1 z] lmin
        [x2 y2 z] lmax]
    (cond 
      (Rand/chance 0.3)
        (let [pillar (Rand/pick ["wall" "pillar"])]
          (as-> game game
            (set-tile game (loc (inc x1) (inc y1) z) (lib/create game pillar))
            (set-tile game (loc (dec x2) (inc y1) z) (lib/create game pillar))
            (set-tile game (loc (inc x1) (dec y2) z) (lib/create game pillar))
            (set-tile game (loc (dec x2) (dec y2) z) (lib/create game pillar))))
      :else 
        (mm/fill-block game 
                       (loc (inc x1) (inc y1) z)
                       (loc (dec x2) (dec y2) z)
                       (lib/create game (Rand/pick ["murky pool" "shallow pool" "deep pool" "magma pool"]))))))
;
(defn decorate-room [game room]
  (and-as-> game game 
    (cond
      (Rand/chance 0.03)
        (decorate-lair game room)
      (Rand/chance 0.2)
        (decorate-normal-room game room)
      (Rand/chance 0.2)
        (decorate-store-room game room (Rand/pick ["[:is-food]" "[:is-potion]" "[:is-mushroom]" "[:is-ingredient]" "[:is-herb]"]))
      (Rand/chance 0.07)
        (decorate-lab game room)
      (Rand/chance 0.1)
        (decorate-designer-room game room)
      :else
        ;; an empty room
      game)
    (ensure-doors game room)))

(defn decorate-rooms [game]
  (reduce 
    decorate-room
    game
    (:rooms game)))

(defn generate-room [game ^mikera.orculje.engine.Location lmin 
                          ^mikera.orculje.engine.Location lmax
                          connections]
  (let [[x1 y1 z] lmin
        [x2 y2 z] lmax]
    (and-as-> game game
      (mm/fill-block game (loc (dec x1) (dec y1) z) (loc (inc x2) (inc y2) z) (lib/create game "wall"))
      (mm/fill-block game lmin lmax (lib/create game "floor"))
      (assoc game :rooms (conj (or (:rooms game) []) {:lmin lmin :lmax lmax :connections connections}))
      (reduce (fn [game con]
                (mm/fill-block game con con (lib/create game "floor"))) game connections))))

(def TUNNEL-DIRS [(loc 1 0 0) (loc -1 0 0) (loc 0 1 0)(loc 0 -1 0)])

(defn generate-tunnel
  [game ^mikera.orculje.engine.Location lmin 
        ^mikera.orculje.engine.Location lmax
        ^mikera.orculje.engine.Location lfrom
        ^mikera.orculje.engine.Location lto
        type]
    (if (= lfrom lto) 
      (mm/fill-block game lfrom lfrom (lib/create game "cave floor"))
      (let [game (mm/fill-block game lfrom lfrom (lib/create game type))
            dir (if (Rand/chance 0.3)
                  (Rand/pick TUNNEL-DIRS)
                  (if (Rand/chance 0.5)
                    (loc 0 (Math/signum (double (- (.y lto) (.y lfrom)))) 0)
                    (loc (Math/signum (double (- (.x lto) (.x lfrom)))) 0 0)))
            nloc (loc-bound lmin lmax (loc-add dir lfrom))]
        (recur game lmin lmax nloc lto type))))

(defn generate-caves [game ^mikera.orculje.engine.Location lmin 
                          ^mikera.orculje.engine.Location lmax
                          connections]
  (let [cloc (rand-loc lmin lmax)]
    (and-as-> game game
      (reduce
        (fn [game c] (generate-tunnel game lmin lmax (loc-bound lmin lmax c) cloc "cave floor"))
        game
        connections)
      (reduce (fn [game con]
        (mm/fill-block game con con (lib/create game "cave floor"))) game connections)
      (if (and (== 1 (count connections)) (Rand/chance 0.5))
        (maybe-place-thing game cloc cloc (lib/create game "[:is-item]" (- (lmin 2))))
        game))))

(defn generate-grid-corridor [game
                            ^mikera.orculje.engine.Location lmin 
                            ^mikera.orculje.engine.Location lmax]
  (let [[x1 y1 z] lmin [x2 y2 z] lmax
        lp (if (Rand/chance 0.5) (loc x1 y2 z) (loc x2 y1 z))]
    (as-> game game
      (mm/fill-block game lmin lp (lib/create game "floor"))
      (mm/fill-block game lp lmax (lib/create game "floor")))))

(defn generate-grid [game ^mikera.orculje.engine.Location lmin 
                          ^mikera.orculje.engine.Location lmax
                          connections]
  (let [[x1 y1 z] lmin [x2 y2 z] lmax
        cloc (rand-loc lmin lmax)]
    (and-as-> game game
      (mm/fill-block game lmin lmax (lib/create game (Rand/pick ["rock wall" "wall"])))
      (reduce
        (fn [game c] 
          (generate-grid-corridor game (loc-bound lmin lmax c) cloc))
        game
        connections)
      (reduce (fn [game con]
        (mm/fill-block game con con (lib/create game "floor"))) game connections))))

(defn generate-block [game ^mikera.orculje.engine.Location lmin 
                          ^mikera.orculje.engine.Location lmax
                          connections]
  (or-loop [10] 
    (cond
      (Rand/chance 0.3) (generate-caves game lmin lmax connections)
      (Rand/chance 0.1) (generate-grid game lmin lmax connections)
      :else (generate-room game lmin lmax connections))))

(defn find-split [split-dir lmin lmax connections]
  (let [[x1 y1 z] lmin
        [x2 y2 z] lmax
        w (inc (- x2 x1))
        h (inc (- y2 y1))
        sw (if (== 0 split-dir) w h)
        sw2 (quot sw 2)]
    (or-loop [20]
      (let [split-point (+ 3 (Rand/r (- sw sw2 3)) (Rand/r (- sw2 3)))
            split-val (+ split-point (lmin split-dir))]
        (if (some (fn [^mikera.orculje.engine.Location l] (== split-val (nth l split-dir))) connections)
          nil
          split-point)))))

(def MAX_BLOCK_SIZE 16)
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
        conw (if (== 0 split-dir) h w) ;; width of connecting wall
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
              new-cons (if (and (> (* conw (Rand/nextDouble)) 10) 
                                (< 1 (loc-dist-manhattan new-con new-con2)))
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
      (generate-tunnel game lmin lmax (rand-loc lmin lmax) (rand-loc lmin lmax) "underground stream")
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



(defn connect-levels 
  ([game lmin lmax]
    (let [[x1 y1 z1] lmin
          [x2 y2 z2] lmax]
      (and-as-> game game
                (reduce 
                  (fn [game i ] 
                    (or-loop [1000]
                      (let [x (Rand/range x1 x2)
                            y (Rand/range y1 y2)]
                        (and game (connect-levels game (loc x y i) (loc x y (inc i)) :link)))))
                  game
                  (range z1 z2)))))
  ([game lmin lmax _]
    (and-as-> game game
              (if (and game (not (get-blocking game lmin)) (not (seq (get-things game lmin))))
                (add-thing game lmin (lib/create game "up staircase")))
              (if (and game (not (get-blocking game lmax)) (not (seq (get-things game lmax))))
                (add-thing game lmax (lib/create game "down staircase"))))))

(defn place-exit-staircase [game lmin lmax]
  (let [[x1 y1 z1] lmin
        [x2 y2 z2] lmax]
    (as-> game game
          (or 
            (or-loop [1000] (mm/place-thing game (loc x1 y1 z2) lmax (lib/create game "exit staircase")))
            (error "Can't place exit staircase!!"))
          (assoc game :start-location (location game (:last-added-id game))))))

(defn place-philosophers-stone [game lmin lmax]
  (let [[x1 y1 z1] lmin
        [x2 y2 z2] lmax]
    (as-> game game
          (or 
            (or-loop [1000] (mm/place-thing game lmin (loc x2 y2 z1) (lib/create game "The Philosopher's Stone")))
            (error "Can't place philosopher's stone!!")))))

(def DUNGEON_MIN (loc -35 -25 -10))
(def DUNGEON_MAX (loc 35 25 0))

(defn generate-dungeon 
  "Attempts to generate dungeon. May return nil on failure" 
  [game]
   (let [lmin DUNGEON_MIN
         lmax DUNGEON_MAX]
     (and-as-> game game
              (assoc game :volume {:min lmin :max lmax})
              (mm/fill-block game (loc-dec lmin) (loc-inc lmax) (lib/create game "rock wall"))
              (generate-region game lmin lmax )
              (place-exit-staircase game lmin lmax)
              (place-philosophers-stone game lmin lmax)
              (decorate-rooms game)
              (connect-levels game lmin lmax)
              (connect-levels game lmin lmax))))

(defn generate 
   "Main dungeon generation algorithm"
  [game]
  (let []
    (loop [game game i 100]
          (or (generate-dungeon game)     
              (when (> i 0)
                (println "Retrying map generation: " i)
                (recur game (dec i)))))))