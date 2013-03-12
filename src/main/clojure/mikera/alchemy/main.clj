(ns mikera.alchemy.main
  (:use mikera.orculje.core)
  (:use mikera.cljutils.error)
  (:use mikera.orculje.util)
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
(def MESSAGE_WINDOW_HEIGHT 5) 

(defn new-frame 
  (^JFrame []
    (let [frame (JFrame.)]
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
    (let [BD 30.0
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
	        gw (int (- w 20))
	        gh (int (- h MESSAGE_WINDOW_HEIGHT))
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
                ^Color fg (or (:colour-fg t) (error "No foreground colour! on " (:name t)))
                ^Color bg (or (:colour-bg t) (error "No foreground colour! on " (:name t)))
                ^Color fg (if visible? (shade-colour fg d2) fg)
                ^Color bg (if visible? (shade-colour bg d2) bg)]
	          (.setForeground jc fg)
	          (.setBackground jc bg)
	          (gui/draw jc x y (char (:char t))))))
	    (.repaint jc))))

(defn redraw-messages [state]
  (let [^JConsole jc (:console state)
	      game @(:game state) 
        w (.getColumns jc)
	      h (.getRows jc)
        mh MESSAGE_WINDOW_HEIGHT
        sy (- h mh)
        msgs (:messages game)
        cm (count msgs)
        more-msgs? (> cm mh)]
    (.fillArea jc \space (colour 0xC0C0C0) (colour 0x000000) (int 0) (int sy) (int w) (int mh))
	  (.setForeground jc ^Color (colour 0xC0C0C0))
	  (.setBackground jc ^Color (colour 0x000000))
    (dotimes [i (if more-msgs? (dec mh) cm)]
      (gui/draw jc 0 (+ sy i) (msgs i)))
    (if more-msgs? (gui/draw jc 0 (+ sy (dec mh)) "[press m to see more messages]"))))

(defn redraw-stats [state]
  (let [^JConsole jc (:console state)
	      game @(:game state) 
        hero (engine/hero game) 
	      w (.getColumns jc)
	      h (.getRows jc)
	      gw 20
	      gh (- h MESSAGE_WINDOW_HEIGHT)
          ]
    (.fillArea jc \space (colour 0xC0C0C0) (colour 0x301020) (int (- w gw)) (int 0) (int gw) (int gh))))

(defn redraw-screen 
  "Redraw the main playing screen"
  ([state]
    (redraw-world state)
    (redraw-messages state)
    (redraw-stats state)))

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

(defn launch 
  "Launch the game with an initial game state. Can be called from REPL."
  ([state]
    (let [^JFrame frame (:frame state)
          ^JConsole jc (:console state)]
      (setup-input jc state) 
      (.add (.getContentPane frame) jc)
      (.pack frame)
      (.setVisible frame true)
      (main-handler state) 
      frame)))

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

;; a state for the world
(def s (new-state))

(defn relaunch 
  "Relaunches the game with a new initial state. Designed for REPL use" 
  ([]
    (def s (new-state))
    (launch s)))

(defn main 
  "Main entry point to the demo, called directly from Java main() method in DemoApp"
  ([]
    (let [^JFrame frame (launch s)]
      (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))))