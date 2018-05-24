(ns mikera.alchemy.main
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util)
  (:require [mikera.cljutils.loops :as loop])
  (:require [mikera.orculje.gui :as gui])
  (:require [mikera.orculje.text :as text])  
  (:require [mikera.alchemy.world :as world])
  (:require [mikera.alchemy.lib :as lib])
  (:require [mikera.alchemy.engine :as engine])
  (:import [javax.swing JFrame JComponent])
  (:import [java.awt.event KeyEvent])
  (:import [java.awt Font Color])
  (:import [mikera.gui JConsole]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^Font font (Font. "Courier New" Font/PLAIN 20))
(def SCREEN_WIDTH 80)
(def SCREEN_HEIGHT 30)
(def RIGHT_AREA_WIDTH 0) 
(def STATUS_BAR_HEIGHT 3) 
(def MAX_DISPLAYED_MESSAGES 5)

(def TEXT_COLOUR (colour 0xC0C0C0))

(declare main-handler) 
(declare restart) 

(defn new-frame 
  (^JFrame []
    (let [frame (JFrame. "Alchemy I : Quest for the Philosopher's Stone")]
      frame)))

(defn new-console
  (^JConsole []
    (let [jc (JConsole. SCREEN_WIDTH SCREEN_HEIGHT)]
      (.setMainFont jc font)
      (.setFont jc font)
      (.setFocusable jc true)
      (.setCursorVisible jc false)
      (.setCursorBlink jc false)
      jc)))

;; ===================================================
;; main GUI display

(defn displayable-thing 
  "Gets the thing that should be displayed in a given square 
   (i.e. highest z-order of all visible things)"
  [game ^long x ^long y ^long z]
  (let [t (or (get-tile game x y z) world/BLANK_TILE)]
    (loop [z-order (long -100)
           ct t
           ts (seq (get-things game x y z))]
      (if ts
        (let [nt (first ts)
              nz (long (:z-order nt 0))]
          (if (and (> nz z-order) (not (:is-invisible nt)))                
            (recur nz nt (next ts))
            (recur z-order ct (next ts))))
        ct))))

(defn shade-colour 
  "Produces a fade to black over distance"
  ([^Color c ^double d2 ^double base]
    (let [BD 50.0
          p (/ BD (+ BD d2))
          p (+ base (* p (- 1.0 base)))
          op 0 
          ;; op (* (- 1.0 p) base)
          ]
      (if (< p 1.0)
        (let [p (/ p 255.0)
              r (float (+ op (* p (.getRed c))))
              g (float (+ op (* p (.getGreen c))))
              b (float (+ op (* p (.getBlue c))))]
          (Color. r g b (float 1.0)))
        c))))

(defn redraw-world 
  "Redraws the game world playing area"
  ([state]
    (redraw-world state (:location (engine/hero @(:game state))) ))
  ([state loc]
	  (let [^JConsole jc (:console state)
	        game @(:game state) 
          hero (engine/hero game) 
          ^mikera.engine.BitGrid viz (or (:visibility game) (error "No visibility defined!"))
          ^mikera.orculje.engine.Location loc (or loc (:location hero)) 
          ^mikera.engine.PersistentTreeGrid disc (or (:discovered-world game) (error "No discovered world?!?")) 
	        w (.getColumns jc)
	        h (.getRows jc)
          hx (.x loc) hy (.y loc) hz (.z loc) 
	        gw (int (- w RIGHT_AREA_WIDTH))
	        gh (int (- h STATUS_BAR_HEIGHT))
          ox (long (- hx (quot gw 2))) 
          oy (long (- hy (quot gh 2))) 
          oz (long hz)]
	    (dotimes [y gh]
	      (dotimes [x gw]
	        (let [tx (int (+ ox x)) 
                ty (int (+ oy y))
                d2 (let [dx (- hx tx) dy (- hy ty)] (+ (* dx dx) (* dy dy)))
                visible? (.get viz tx ty hz)
                dtile (.get disc tx ty hz)
                base (if (and dtile (not (:is-floor dtile))) 0.35 0.0)
                t (if visible?
                    (displayable-thing game tx ty oz)
                    (or dtile world/UNSEEN_TILE))
                t (or t world/BLANK_TILE)
                base (if (:is-unseen-tile t) 0.5 base)
                ^Color fg (or (? t :colour-fg) (error "No foreground colour! on " (str t)))
                ^Color bg (or (? t :colour-bg) (error "No foreground colour! on " (str t)))
                ^Color fg (if visible? (shade-colour fg d2 base) (shade-colour fg 1000000.0 base))
                ^Color bg (if visible? (shade-colour bg d2 base) (shade-colour bg 1000000.0 base))]
	          (.setForeground jc fg)
	          (.setBackground jc bg)
	          (gui/draw jc x y (char (? t :char))))))
	    (.repaint jc))))

(defn redraw-messages [state]
  (let [^JConsole jc (:console state)
	      game @(:game state) 
        w (.getColumns jc)
	      h (.getRows jc)
        msgs (:messages game)
        cm (count msgs)
        mh MAX_DISPLAYED_MESSAGES
        sy 0
        more-msgs? (> cm mh)]
    (.fillArea jc \space TEXT_COLOUR (colour 0x000000) (int 0) (int sy) (int w) (min cm mh))
	  (.setForeground jc ^Color TEXT_COLOUR)
	  (.setBackground jc ^Color (colour 0x000000))
    (dotimes [i (if more-msgs? (dec mh) cm)]
      (gui/draw jc 1 (+ sy i) (msgs i)))
    (if more-msgs? (gui/draw jc 1 (+ sy (dec mh)) "[press m to see more messages]"))))

(def STAT_BACKGROUND_COLOUR (colour 0x301020))
(def STAT_SPACE "  ")

(defn redraw-stats [state]
  (let [^JConsole jc (:console state)
	      game @(:game state) 
        hero (engine/hero game) 
        loc (location game hero) 
	      w (.getColumns jc)
	      h (.getRows jc)
	      sy (- h STATUS_BAR_HEIGHT) 
          ]
    (.fillArea jc \space TEXT_COLOUR STAT_BACKGROUND_COLOUR (int 0) sy (int w) (int STATUS_BAR_HEIGHT))
    (let [hps (:hps hero)
          hpsmax (:hps-max hero)
          p (max 0.0 (min 1.0 (/ (double hps) hpsmax)))]
      (.setBackground jc ^Color STAT_BACKGROUND_COLOUR)
      (.setForeground jc ^Color TEXT_COLOUR)
      (gui/draw jc 1 (+ sy 0) (str "HPs: "))
      (.setForeground jc ^Color (Color. (float (min 1.0 (- 2 (* 2 p))))
                                        (float (min 1.0 (* 2 p)))
                                        (float 0)))
	    (gui/draw jc 6 (+ sy 0) (str hps "/" hpsmax))
      (.setForeground jc ^Color TEXT_COLOUR)
      (gui/draw jc 16 (+ sy 0) (str (str "SK:" (? game hero :SK) STAT_SPACE)
                                    (str "ST:" (? game hero :ST) STAT_SPACE)
                                    (str "AG:" (? game hero :AG) STAT_SPACE)
                                    (str "TG:" (? game hero :TG) STAT_SPACE)
                                    (str "IN:" (? game hero :IN) STAT_SPACE)
                                    (str "WP:" (? game hero :WP) STAT_SPACE)
                                    (str "CH:" (? game hero :CH) STAT_SPACE)
                                    (str "CR:" (? game hero :CR) STAT_SPACE)))
      (.setForeground jc ^Color TEXT_COLOUR)
      (gui/draw jc 1 (+ sy 1) (str "Lvl: " (- (loc 2)) "/10"))
      (.setForeground jc ^Color TEXT_COLOUR)
      (gui/draw jc 16 (+ sy 1) (str (str "ARM:" (? game hero :ARM) STAT_SPACE)
                                    (str "Speed:" (? game hero :speed) STAT_SPACE)))
      (.setForeground jc ^Color TEXT_COLOUR)
      (let [effs (filter :is-effect (contents hero))
            eff-string (apply str (distinct (map #(str (text/capitalise (:name %)) "  ") effs)))]
        (gui/draw jc 1 (+ sy 2) (text/truncate-with-dots eff-string (- w 2)))))))

(defn redraw-screen 
  "Redraw the main playing screen"
  ([state]
    (redraw-world state)
    (redraw-messages state)
    (redraw-stats state)))

;; =======================================================
;; action handling macro

(defmacro handle-turn [& actions]
  `(as-> ~'game ~'game
     (engine/clear-messages ~'game)
     ~@actions 
     (world/end-turn ~'game)))

;; ========================================================
;; command
;
(def COMMANDS
  [["Direction keys" "Move around map / attack / open doors"]
   []
   ["a" "Use an analysis lab to identify items and their composition"]
   ["c" "Use an alchemy workbench to craft potions"]
   ["d" "Drop an item"]
   ["e" "Eat a food item"]
   ["i" "Examine your inventory"]
   ["l" "Look around (movement keys to move cursor)"]
   ["m" "Show recent messages"]
   ["o" "Open / close a door, box or mechanism"]
   ["p" "Pick up an item"]
   ["q" "Quaff potion"]
   ["t" "Throw something"]
   ["<" "Go up stairs / ramp"]
   [">" "Go down stairs/ramp"]
   ["," "Quick pickup (picks up first available item on floor)"]
   ["." "Wait a short time at this location"]
   []
   ["R" "Restart the game"]
   ["?" "Show this screen (command help)"]
  ])

(defn show-commands [state]
  (let [^JConsole jc (:console state)
	      w (.getColumns jc)
	      h (.getRows jc)]
    (.fillArea jc \space TEXT_COLOUR (colour 0x002010) 0 0 w h)
    (.setForeground jc ^Color TEXT_COLOUR)
    (.setBackground jc ^Color (colour 0x002010))   
    (gui/draw jc 1 0 "Alchemy help: commands")
    (dotimes [i (count COMMANDS)]
      (when (seq (COMMANDS i))
        (gui/draw jc 3 (+ 2 i) (str ((COMMANDS i) 0) "  =  " ((COMMANDS i) 1)))))

    (reset! (:event-handler state)
	          (fn [^String k]
	            (let [sel (.indexOf "abcdefghijklmnopqrstuvwxyz" k)]
	              (cond 
	                :else 
	                  (main-handler state))))))) 

;; ========================================================
;; messages

(defn show-messages [state]
  (let [^JConsole jc (:console state)
	      w (.getColumns jc)
	      h (.getRows jc)
        msg-log (:message-log @(:game state))
        msg-list (vec (take 26 (mapcat 
                                 (fn [[& ms] i] (map vector ms (repeat i))) 
                                 msg-log
                                 (range))))]
    (.fillArea jc \space TEXT_COLOUR (colour 0x010020) 0 0 w h)
    (.setForeground jc ^Color TEXT_COLOUR)
    (.setBackground jc ^Color (colour 0x010020))   
    (gui/draw jc 1 0 "Recent message log:")
    (dotimes [i (min (- SCREEN_HEIGHT 3) (count msg-list))]
      (let [[ms j] (msg-list i)
            p (float (/ 0.8 (+ 0.8 (* 0.2 j))))]
        (.setForeground jc ^Color (Color. p p p))
        (gui/draw jc 3 (+ 2 i) ms)))
    (reset! (:event-handler state)
	          (fn [^String k]
	            (cond 
	                :else 
	                  (main-handler state)))))) 

;; ========================================================
;; item selection

(defn redraw-item-select-screen [state msg items pos]
  (let [^JConsole jc (:console state)
	      w (.getColumns jc)
	      h (.getRows jc)
        c (count items)]
    (.fillArea jc \space TEXT_COLOUR (colour 0x201000) 0 0 w h)
    (.setForeground jc ^Color TEXT_COLOUR)
    (.setBackground jc ^Color (colour 0x201000))
    (gui/draw jc 1 0 msg)
    (loop/for-loop [i 0 (and (< (+ pos i) c) (< i 26)) (inc i)]
      (.setForeground jc ^Color TEXT_COLOUR)
      (gui/draw jc 3 (+ 2 i) (str "[ # ] = " (items (+ pos i))))
      (.setForeground jc ^Color (colour 0xFFFF80))
      (gui/draw jc 5 (+ 2 i) (char (+ (int \a) i))))
    (.setForeground jc ^Color TEXT_COLOUR)
    (gui/draw jc 0 29 (str " Page " (inc (quot pos 26)) " of " (inc (quot (dec c) 26)) "   "
                           (if (> pos 0) "[UP to go back]  " "")
                           (if (< (+ pos 25) c) "[DOWN for more]  " "")))
    (.repaint jc)))

(def last-item-pos (atom 0))
(defn select-item-pos [inv]
  (let [old-pos (long @last-item-pos)
        c (count inv)]
    (if (< old-pos c)
      old-pos
      0)))

(defn item-select-handler [state msg items pos action]
  (reset! last-item-pos pos)
  (let [c (count items)]
    (redraw-item-select-screen state msg items pos)
	  (reset! (:event-handler state)
	          (fn [^String k]
	            (let [sel (.indexOf "abcdefghijklmnopqrstuvwxyz" k)]
	              (cond 
	                (and (>= sel 0) (< (+ pos sel) c))
	                  (action (+ sel pos))
	                (and (> pos 0) (.contains "784" k))
                    (item-select-handler state msg items (- pos 26) action)
                  (and (< (+ pos 26) c) (.contains "623" k))
                    (item-select-handler state msg items (+ pos 26) action)  
	                (= "Q" k)
	                  (main-handler state)
	                :else 
	                  :ignored))))))

;; =====================================================
;; direction selection

(def move-dir-map
  {"1" (loc -1 1 0)
   "2" (loc 0 1 0)
   "3" (loc 1 1 0)
   "4" (loc -1 0 0)
   "5" (loc 0 0 0)
   "6" (loc 1 0 0)
   "7" (loc -1 -1 0)
   "8" (loc 0 -1 0)
   "9" (loc 1 -1 0)
   "<" (loc 0 0 1)
   ">" (loc 0 0 -1)})

(defn direction-select-handler [state msg action]
  (let [^JConsole jc (:console state)
        w (.getColumns jc)
	      h (.getRows jc)
       game @(:game state)
       ]
    (redraw-world state (engine/hero-location game))
    (redraw-stats state)
    (.fillArea jc \space TEXT_COLOUR (colour 0x200020) 0 0 w 1)
    (.setForeground jc ^Color TEXT_COLOUR)
    (.setBackground jc ^Color (colour 0x200020))
    (gui/draw jc 1 0 msg)
    (reset! (:event-handler state)
	          (fn [^String k]
	            (let [sel (.indexOf "123456789" k)]
	              (cond 
	                (>= sel 0)
	                  (action (move-dir-map k))
	                (= "Q" k)
	                  (main-handler state)
	                :else 
	                  :ignored))))
))

;; ================================================================
;; map selection

(defn draw-crosshairs [^JConsole jc cx cy]
  (let [^Color fg (.getForegroundAt jc cx cy)
        ^Color bg (.getBackgroundAt jc cx cy)
        ch (char (.getCharAt jc cx cy))]
    (.setCursorPos jc cx cy)
    (.setForeground jc ^Color bg)
    (.setBackground jc ^Color fg)
    (.write jc ch)
;    (.setForeground jc (colour 0xFFA080))
;    (.setBackground jc (colour 0x000000))
;    (gui/draw jc (dec cx) (dec cy) "\\")
;    (gui/draw jc (dec cx) (inc cy) "/")
;    (gui/draw jc (inc cx) (dec cy) "/")
;    (gui/draw jc (inc cx) (inc cy) "\\")
    ))

(defn map-select-handler [state msg loc action]
  (let [^JConsole jc (:console state)
        game @(:game state)
        loc (location game loc)
        w (.getColumns jc)
	      h (.getRows jc)
        [x y z] loc 
       	gw (int (- w RIGHT_AREA_WIDTH))
	      gh (int (- h STATUS_BAR_HEIGHT))
        cx (int (quot gw 2)) 
        cy (int (quot gh 2)) 
        ^mikera.engine.BitGrid viz (or (:visibility game) (error "No visibility defined!"))
        ^mikera.engine.PersistentTreeGrid disc (or (:discovered-world game) (error "No discovered world?!?")) 
        visible? (.get viz (int x) (int y) (int z))
        dthing (if visible?
                 (displayable-thing game (int x) (int y) (int z))
                 (or (.get disc x y z) world/UNSEEN_TILE))
       ]
    (redraw-world state loc)
    (redraw-stats state)
    (.fillArea jc \space TEXT_COLOUR (colour 0x200000) 0 0 w 2)
    (.setForeground jc ^Color TEXT_COLOUR)
    (.setBackground jc ^Color (colour 0x200000))
    (gui/draw jc 1 0 msg)
    (gui/draw jc 3 1 
              (mikera.orculje.text/capitalise (engine/base-name game dthing))) 
    (draw-crosshairs jc cx cy)
    (reset! (:event-handler state)
	          (fn [^String k]
               (let [sel (.indexOf "123456789<>" k)]
	              (cond 
	                (>= sel 0)
	                  (map-select-handler state msg (loc-add loc (move-dir-map k)) action )
	                (and (= " " k) (engine/is-square-visible? game loc))
                    (action loc)
                  (= "Q" k)
                    (main-handler state)
                  :else 
                    :ignored))))))

;; ================================================================
;; manipulation actions

(defn choose-open [state]
  (let [game @(:game state)
        hero (engine/hero game)]
    (direction-select-handler state "Open: select direction" 
                              (fn [dir] 
                                (swap! (:game state) world/handle-open dir)
                                (main-handler state)))))

;; ================================================================
;; inventory actions

(defn choose-alchemy [state]
  (let [game @(:game state)
        hero (engine/hero game)
        inv (vec (lib/all-library-things game 
                   (fn [p] (and (:is-potion p) (:is-identified p) (:is-recipe-known p)))))]
    (item-select-handler state "Create a potion:" 
                      (vec (map 
                             (fn [p] 
                               (str
                                 (engine/base-name game p)
                                 (if (engine/has-ingredients? game hero p)
                                   " [OK]" " [ingred. missing]"))) inv))
                      (select-item-pos inv) 
                      (fn [n] 
                        (swap! (:game state) world/handle-alchemy (inv n))
                        (main-handler state))))) 


(defn choose-analyse [state]
  (let [game @(:game state)
        hero (engine/hero game)
        inv (vec (filter 
                   (fn [p] (and (or (:is-potion p) (:is-ingredient p))
                                (or (not (is-identified? game p))
                                    (and (:is-potion p) (not (engine/is-recipe-known? game p))))))
                   (contents hero)))]
    (cond
      (== 0 (count inv))
        (do 
          (swap! (:game state) world/message "You have no items to analyse.")
          (main-handler state))
      (find-nearest-thing game (name-pred "analysis lab") hero 1)
        (item-select-handler state "Analyse an item:" 
                           (vec (map (partial engine/base-name game) inv))
                           (select-item-pos inv) 
                           (fn [n] 
                             (swap! (:game state) world/handle-analyse (inv n))
                             (main-handler state)))
      :else 
        (do 
          (swap! (:game state) world/message "You need to find an analysis lab to analyse items.")
          (main-handler state))))) 

(defn choose-drop [state]
  (let [game @(:game state)
        hero (engine/hero game)
        inv (vec (filter :is-item (contents hero)))]
    (item-select-handler state "Drop an item:" 
                      (vec (map (partial engine/base-name game) inv))
                      (select-item-pos inv) 
                      (fn [n] 
                        (swap! (:game state) world/handle-drop (inv n))
                        (main-handler state))))) 

(defn choose-eat [state]
  (let [game @(:game state)
        hero (engine/hero game)
        inv (vec (filter :is-food (contents hero)))]
    (item-select-handler state "Eat an item:" 
                      (vec (map (partial engine/base-name game) inv))
                      (select-item-pos inv) 
                      (fn [n] 
                        (swap! (:game state) world/handle-consume (inv n))
                        (main-handler state))))) 


(defn choose-quaff [state]
  (let [game @(:game state)
        hero (engine/hero game)
        inv (vec (filter :is-potion (contents hero)))]
    (item-select-handler state "Quaff a potion:" 
                      (vec (map (partial engine/base-name game) inv))
                      (select-item-pos inv) 
                      (fn [n] 
                        (swap! (:game state) world/handle-consume (inv n))
                        (main-handler state))))) 

(defn show-inventory [state]
  (let [game @(:game state)
        hero (engine/hero game)
        inv (vec (filter :is-item (contents hero)))]
    (item-select-handler state "Examine your inventory:" 
                      (vec (map (partial engine/base-name game) inv))
                      (select-item-pos inv) 
                      (fn [n] (main-handler state))))) 

(defn choose-pickup [state]
  (let [game @(:game state)
        inv (vec (filter :is-item (get-things game (engine/hero-location game))))]
    (cond 
      (== 0 (count inv))
        (do
          (swap! (:game state) world/message "There is nothing here to pick up.")
          (main-handler state))
      (== 1 (count inv))
        (do 
          (swap! (:game state) world/handle-pickup (inv 0))
          (main-handler state))
      :else (item-select-handler state "Pick up an item:" 
                      (vec (map (partial engine/base-name game) inv))
                      0
                      (fn [n] 
                        (swap! (:game state) world/handle-pickup (inv n))
                        (main-handler state)))))) 

(defn quick-pickup [state]
  (let [game @(:game state)
        inv (vec (filter :is-item (get-things game (engine/hero-location game))))]
    (cond 
      (== 0 (count inv))
        (swap! (:game state) world/message "There is nothing here to pick up.")
      :else (do 
              (swap! (:game state) world/handle-pickup (inv 0))
              (main-handler state))))) 

(defn choose-throw-target [state missile]
  (let [game @(:game state)]
     (map-select-handler state 
                       "Select a target:" 
                       (if-let [tt (find-nearest-thing game (fn [t] (and 
                                                                      (:is-creature t)
                                                                      (not (:is-hero t))
                                                                      (engine/is-square-visible? game t)))
                                                       (engine/hero game) 15)] 
                         tt
                         (engine/hero-location game)) 
                       (fn [tloc] 
                         (swap! (:game state) world/handle-throw missile tloc)
                         (main-handler state)))))

(defn choose-throw [state]
  (let [game @(:game state)
        hero (engine/hero game)
        inv (vec (filter :is-item (contents hero)))]
    (item-select-handler state "Select an item to throw:" 
                      (vec (map (partial engine/base-name game) inv))
                      (select-item-pos inv) 
                      (fn [n] 
                        (choose-throw-target state (inv n)))))) 

 (defn do-look [state]
   (let [game @(:game state)]
     (map-select-handler state 
                       "Look around:" 
                       (engine/hero-location game) 
                       (fn [n] 
                         (main-handler state))))) 

;;=========================================================
;; special actions
(defn do-confirm [state msg action]
  (let [^JConsole jc (:console state)
        game @(:game state)
        w (.getColumns jc)
	      h (.getRows jc)
       CONFIRM-BG-COLOUR (colour 0x400000)
       ]
    (redraw-world state (engine/hero-location game))
    (redraw-stats state)
    (.fillArea jc \space TEXT_COLOUR CONFIRM-BG-COLOUR 0 0 w 1)
    (.setForeground jc ^Color TEXT_COLOUR)
    (.setBackground jc ^Color CONFIRM-BG-COLOUR)
    (gui/draw jc 1 0 msg)
    (reset! (:event-handler state)
	          (fn [^String k]
               (cond 
	                (.contains "Yy" k)
                    (action)
                  (.contains "NnQ " k)
                    (main-handler state)
                  :else 
                    :ignored)))))
 
;; ========================================================
;; Input state handler functions
;;
;; each handler sets up :event-handler to deal with next keypress


(defn map-synonyms [k]
  ({"5" "."} k k))


(defn make-main-handler
  "Create main keypress handler, for general game position"
  ([state]
    (fn [k]
      (let [game @(:game state)
            k (map-synonyms k) 
            game-over? (:game-over game)]
        (cond
          (or (= "R" k) (and game-over? (= "r" k))) 
                    (if game-over?
                      (restart state)
                      (do-confirm state "Restart game: are you sure (y/n)" 
                                  #(restart state)))
          (= "?" k) (show-commands state)
          (= "i" k) (show-inventory state)
          (= "l" k) (do-look state)
          (= "m" k) (show-messages state)
          game-over? :handled  ;; stop here if game is over
          
          (.contains "12346789<>" k)
            (do
              (swap! (:game state) world/handle-move (or (move-dir-map k) 
                                                         (error "direction not recognised [" k "]")))
              (redraw-screen state))
          (.contains ".5" k) 
            (do
              (swap! (:game state) world/handle-wait 100)
              (redraw-screen state))
          (= "a" k) (choose-analyse state)
          (= "c" k) (choose-alchemy state)
          (= "d" k) (choose-drop state)
          (= "e" k) (choose-eat state)
          (= "p" k) (choose-pickup state)
          (= "q" k) (choose-quaff state)
          (= "o" k) (choose-open state)
          (= "," k) (quick-pickup state)
          (= "t" k) (choose-throw state)
          
          :else
	          (do 
	            (swap! (:game state) world/handle-command k)
	            (redraw-screen state)))
        :handled))))

(defn main-handler 
  "Sets up the main handler"
  ([state]
    (redraw-screen state)
    (reset! (:event-handler state) (make-main-handler state)))) 

;; ====================================================
;; input handling
;;
;; Links key presses to calls of the current :event-handler

(defn make-input-action 
  "Builds an input action handler for the specified state object"
  ([state k]
    (fn []
      (locking state
        (let [hand @(:event-handler state)]
          (or
            (try (hand k)
              (catch Throwable t
                (do 
                  (swap! (:game state) assoc :error t)
                  (throw t))))
            (println (str "Key pressed but no event handler ready: " k))))))))

(defn setup-input 
  ([^JComponent comp state]
    (doseq [k "abcdefghijklmnopqrstuvwxyz 01234567890!\"\\/£$%^&*()'~<>?@#_-+=[]{},."]
      (gui/add-input-binding comp (gui/keystroke k) (make-input-action state (str k))))
    (doseq [k "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
      (gui/add-input-binding comp (gui/keystroke k) (make-input-action state (str k))))
    (doseq [[^KeyEvent ke k] {KeyEvent/VK_LEFT "4"
                              KeyEvent/VK_RIGHT "6"
                              KeyEvent/VK_UP "8";
                              KeyEvent/VK_DOWN "2"
                              KeyEvent/VK_ESCAPE "Q"}]
      (gui/add-input-binding comp (gui/keystroke-from-keyevent ke) (make-input-action state (str k))))))

;; =================================================================
;; Overall game control / main entry points

(defn restart [state]
  (reset! (:game state) (world/new-game))
  (main-handler state))

(defn new-state
  "Create a brand new game state."
  ([]
    (let [game (world/new-game)
          state {:game (atom game)
                 :console (new-console)
                 :frame (new-frame)
                 :event-handler (atom nil)}]
      state)))

(defn launch 
  "Launch the game with an initial game state. Can be called from REPL."
  ([]
    (def s (new-state))
    (launch s))
  ([state]
    (let [^JFrame frame (:frame state)
          ^JConsole jc (:console state)]
      (setup-input jc state) 
      (.add (.getContentPane frame) jc)
      (.pack frame)
      (.setVisible frame true)
      (main-handler state) 
      frame)))

(defn relaunch 
  "Relaunches the game with a new initial state. Designed for REPL use" 
  ([]
    (launch)))

(defn main 
  "Main entry point to the demo, called directly from Java main() method in DemoApp"
  ([]
    (def s (new-state))
    (let [^JFrame frame (launch s)]
      (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))))

(defmacro dog [& body]
  `(let [~'game @(:game ~'s)
         ~'hero (engine/hero ~'game)]
     ~@body))

(defmacro go [& body]
  `(dog
     (reset! (:game ~'s) 
       (as-> ~'game ~'game
             ~@body))
     (main-handler ~'s)))

(comment
  ;; launch in REPL mode
  (launch)
  
  (keys s)
  
  ;; event handler, trampoline patterm
  (:event-handler s)
  
  (keys @(:game s))
  
  (engine/hero @(:game s))
  
  (def hl (engine/hero-location @(:game s)))
  
  (get-tile @(:game s) hl)
  
  ;; get the current game
  (def g @(:game s))
  ;; save a state
  (def original-g g)
  ;; do some stuff
  (reset! (:game s) original-g)
  (redraw-screen s)
  )
