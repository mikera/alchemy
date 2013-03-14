(ns mikera.alchemy.main
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util)
  (:require [mikera.cljutils.loops :as loop])
  (:require [mikera.orculje.gui :as gui])
  (:require [mikera.alchemy.world :as world])
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
(def STATUS_BAR_HEIGHT 2) 
(def MAX_DISPLAYED_MESSAGES 5)

(declare main-handler) 

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
          (if (and (> nz z-order) (:is-visible nt))
            (recur nz nt (next ts))
            (recur z-order ct (next ts))))
        ct))))

(defn shade-colour 
  "Produces a fade to black over distance"
  ([^Color c ^double d2]
    (let [BD 50.0
          p (/ BD (+ BD d2))]
      (if (< p 1.0)
        (let [p (/ p 255.0)
              r (float (* p (.getRed c)))
              g (float (* p (.getGreen c)))
              b (float (* p (.getBlue c)))]
          (Color. r g b (float 1.0)))
        c))))

(defn redraw-world 
  "Redraws the game world playing area"
  ([state]
	  (let [^JConsole jc (:console state)
	        game @(:game state) 
          hero (engine/hero game) 
          ^mikera.engine.BitGrid viz (or (:visibility game) (error "No visibility defined!"))
          ^mikera.orculje.engine.Location hloc (:location hero) 
	        w (.getColumns jc)
	        h (.getRows jc)
          hx (.x hloc) hy (.y hloc) hz (.z hloc) 
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
                t (if visible?
                    (displayable-thing game tx ty oz)
                    world/BLANK_TILE)
                t (or t world/BLANK_TILE)
                ^Color fg (or (? t :colour-fg) (error "No foreground colour! on " (:name t)))
                ^Color bg (or (? t :colour-bg) (error "No foreground colour! on " (:name t)))
                ^Color fg (if visible? (shade-colour fg d2) fg)
                ^Color bg (if visible? (shade-colour bg d2) bg)]
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
    (.fillArea jc \space (colour 0xC0C0C0) (colour 0x000000) (int 0) (int sy) (int w) (min cm mh))
	  (.setForeground jc ^Color (colour 0xC0C0C0))
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
	      w (.getColumns jc)
	      h (.getRows jc)
	      sy (- h STATUS_BAR_HEIGHT) 
          ]
    (.fillArea jc \space (colour 0xC0C0C0) STAT_BACKGROUND_COLOUR (int 0) sy (int w) (int STATUS_BAR_HEIGHT))
    (let [hps (:hps hero)
          hpsmax (:hps-max hero)
          p (max 0.0 (min 1.0 (/ (double hps) hpsmax)))]
      (.setBackground jc ^Color STAT_BACKGROUND_COLOUR)
      (.setForeground jc ^Color (colour 0xC0C0C0))
      (gui/draw jc 1 (+ sy 0) (str "HPs: "))
      (.setForeground jc ^Color (Color. (float (min 1.0 (- 2 (* 2 p))))
                                        (float (min 1.0 (* 2 p)))
                                        (float 0)))
	    (gui/draw jc 6 (+ sy 0) (str hps "/" hpsmax))
      (.setForeground jc ^Color (colour 0xC0C0C0))
      (gui/draw jc 16 (+ sy 0) (str (str "SK:" (? game hero :SK) STAT_SPACE)
                                    (str "ST:" (? game hero :ST) STAT_SPACE)
                                    (str "AG:" (? game hero :IN) STAT_SPACE)
                                    (str "TG:" (? game hero :WP) STAT_SPACE)
                                    (str "IN:" (? game hero :IN) STAT_SPACE)
                                    (str "WP:" (? game hero :WP) STAT_SPACE)
                                    (str "CH:" (? game hero :CH) STAT_SPACE)
                                    (str "CR:" (? game hero :CR) STAT_SPACE))))))

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

(def COMMANDS
  [["d" "Drop an item"]
   ["e" "Eat a food item"]
   ["i" "Inventory (select an item to examine it)"]
   ["q" "Quaff potion"]])

(defn show-commands [state]
  (let [^JConsole jc (:console state)
	      w (.getColumns jc)
	      h (.getRows jc)]
    (.fillArea jc \space (colour 0xC0C0C0) (colour 0x002010) 0 0 w h)
    (.setForeground jc ^Color (colour 0xC0C0C0))
    (.setBackground jc ^Color (colour 0x002010))   
    (gui/draw jc 1 0 "Alchemy help: commands")
    (dotimes [i (count COMMANDS)]
      (gui/draw jc 3 (+ 2 i) (str ((COMMANDS i) 0) "  =  " ((COMMANDS i) 1))))

    (reset! (:event-handler state)
	          (fn [^String k]
	            (let [sel (.indexOf "abcdefghijklmnopqrstuvwxyz" k)]
	              (cond 
	                :else 
	                  (main-handler state))))))) 

;; ========================================================
;; item selection

(defn redraw-item-select-screen [state msg items pos]
  (let [^JConsole jc (:console state)
	      w (.getColumns jc)
	      h (.getRows jc)
        c (count items)]
    (.fillArea jc \space (colour 0xC0C0C0) (colour 0x201000) 0 0 w h)
    (.setForeground jc ^Color (colour 0xC0C0C0))
    (.setBackground jc ^Color (colour 0x201000))
    (gui/draw jc 1 0 msg)
    (loop/for-loop [i 0 (and (< (+ pos i) c) (< i 26)) (inc i)]
      (.setForeground jc ^Color (colour 0xC0C0C0))
      (gui/draw jc 3 (+ 2 i) (str "[ # ] = " (items (+ pos i))))
      (.setForeground jc ^Color (colour 0xFFFF80))
      (gui/draw jc 5 (+ 2 i) (char (+ (int \a) i))))
    (.setForeground jc ^Color (colour 0xC0C0C0))
    (gui/draw jc 0 29 (str " Page " (inc (quot pos 26)) " of " (inc (quot (dec c) 26)) "   "
                           (if (> pos 0) "[UP to go back]  " "")
                           (if (< (+ pos 25) c) "[DOWN for more]  " "")))
    (.repaint jc)))


(defn item-select-handler [state msg items pos action]
  (let [c (count items)]
    (redraw-item-select-screen state msg items pos)
	  (reset! (:event-handler state)
	          (fn [^String k]
	            (let [sel (.indexOf "abcdefghijklmnopqrstuvwxyz" k)]
	              (cond 
	                (and (>= sel 0) (< (+ pos sel) c))
	                  (action (+ sel pos))
	                (and (> pos 0) (.contains "124" k))
                    (item-select-handler state msg items (- pos 26) action)
                  (and (< (+ pos 26) c) (.contains "689" k))
                    (item-select-handler state msg items (+ pos 26) action)  
	                (= "Q" k)
	                  (main-handler state)
	                :else 
	                  :ignored))))))

(defn show-inventory [state]
  (let [game @(:game state)
        hero (engine/hero game)
        inv (vec (filter :is-item (contents hero)))]
    (item-select-handler state "Examine your inventory:" 
                      (vec (map (partial engine/base-name game) inv))
                      0
                      (fn [n] (main-handler state))))) 

(defn choose-drop [state]
  (let [game @(:game state)
        hero (engine/hero game)
        inv (vec (filter :is-item (contents hero)))]
    (item-select-handler state "Drop an item:" 
                      (vec (map (partial engine/base-name game) inv))
                      0
                      (fn [n] 
                        (swap! (:game state) world/handle-drop (inv n))
                        (main-handler state))))) 

(defn choose-pickup [state]
  (let [game @(:game state)
        inv (vec (filter :is-item (get-things game (engine/hero-location game))))]
    (item-select-handler state "Pick up an item:" 
                      (vec (map (partial engine/base-name game) inv))
                      0
                      (fn [n] 
                        (swap! (:game state) world/handle-pickup (inv n))
                        (main-handler state))))) 

;; ========================================================
;; Input state handler functions
;;
;; each handler sets up :event-handler to deal with next keypress

(def move-dir-map
  {"1" (loc -1 -1 0)
   "2" (loc 0 -1 0)
   "3" (loc 1 -1 0)
   "4" (loc -1 0 0)
   "6" (loc 1 0 0)
   "7" (loc -1 1 0)
   "8" (loc 0 1 0)
   "9" (loc 1 1 0)})

(defn map-synonyms [k]
  ({"5" "."} k k))


(defn make-main-handler
  "Create main keypress handler, for general game position"
  ([state]
    (fn [k]
      (let [k (map-synonyms k) ]
        (cond
          (.contains "12346789" k)
            (do
              (swap! (:game state) world/handle-move (or (move-dir-map k) (error "direction no recognised [" k "]")))
              (redraw-screen state))
          (.contains ".5" k) 
            (do
              (swap! (:game state) world/handle-wait 100)
              (redraw-screen state))
          (= "i" k) (show-inventory state)
          (= "d" k) (choose-drop state)
          (.contains ",p" k) (choose-pickup state)
          (= "?" k) (show-commands state)
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
      (let [hand @(:event-handler state)]
        (or
          (hand k)
          (println (str "Key pressed but no event handler ready: " k)))))))

(defn setup-input 
  ([^JComponent comp state]
    (doseq [k "abcdefghijklmnopqrstuvwxyz 01234567890!\"\\/£$%^&*()'~<>?@#_-+=[]{},."]
      (gui/add-input-binding comp (gui/keystroke k) (make-input-action state (str k))))
    (doseq [k "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
      (gui/add-input-binding comp (gui/keystroke k) (make-input-action state (str k))))
    (doseq [[^KeyEvent ke k] {KeyEvent/VK_LEFT "4"
                              KeyEvent/VK_RIGHT "6"
                              KeyEvent/VK_UP "2"
                              KeyEvent/VK_DOWN "8"
                              KeyEvent/VK_ESCAPE "Q"}]
      (gui/add-input-binding comp (gui/keystroke-from-keyevent ke) (make-input-action state (str k))))))

;; =================================================================
;; Overall game control / main entry points


(defn new-state
  "Create a brand new game state."
  ([]
    (let [game (world/new-game)
          state {:game (atom game)
                 :console (new-console)
                 :frame (new-frame)
                 :view-pos (engine/hero-location game) 
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