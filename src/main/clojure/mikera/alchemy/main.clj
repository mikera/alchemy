(ns mikera.alchemy.main
  (:use mikera.orculje.core)
  (:require [mikera.orculje.gui :as gui])
  (:require [mikera.alchemy.world :as world])
  (:import [javax.swing JFrame JComponent])
  (:import [java.awt.event KeyEvent])
  (:import [java.awt Font Color])
  (:import [mikera.gui JConsole]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^Font font (Font. "Courier New" Font/PLAIN 16))

(defn new-frame 
  (^JFrame []
    (let [frame (JFrame.)]
      frame)))

(defn new-console
  (^JConsole []
    (let [jc (JConsole. 100 40)]
      (.setMainFont jc font)
      (.setFocusable jc true)
      (.setCursorVisible jc false)
      (.setCursorBlink jc false)
      jc)))

(defn displayable-thing 
  [game ^long x ^long y ^long z]
  (let [t (or (get-tile game x y z) world/BLANK_TILE)]
    (loop [z-order (long -100)
           ct t
           ts (seq (get-things game x y z))]
      (when (== 0 x y z) (println "Things at 0,0,0 = " ts))
      (if ts
        (let [nt (first ts)
              nz (long (:z-order nt 0))]
          (println (str "found: " nt))
          (if (and (> nz z-order) (:is-visible nt))
            (recur nz nt (next ts))
            (recur z-order ct (next ts))))
        ct))))

(defn redraw-screen [state]
  (let [^JConsole jc (:console state)
        game @(:game state) 
        w (.getColumns jc)
        h (.getRows jc)
        gw (- w 20)
        gh (- h 5)]
    (dotimes [y gh]
      (dotimes [x gw]
        (let [t (displayable-thing game x y 0)]
          (.setForeground jc ^Color (:colour-fg t))
          (.setBackground jc ^Color (:colour-bg t))
          (gui/draw jc x y (char (:char t))))))
    (.repaint jc)))


(defn make-input-action 
  "Builds an input action handler for the specified state object"
  ([state k]
    (fn []
      (let [hand @(:event-handler state)]
        (or
          (hand k)
          (println (str "Key pressed but no event handler ready: " k)))))))

(defn make-main-handler
  [state]
  (fn [k]
    (let [k ({"5" "."} k k) ;; handle synonyms
          ]
      (swap! (:game state) world/handle-command k)
      (redraw-screen state)
      :handled)))

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

(defn launch 
  "Launch the game with an initial game state. Can be called from REPL."
  ([state]
    (let [^JFrame frame (:frame state)
          ^JConsole jc (:console state)]
      (setup-input jc state) 
      (.add (.getContentPane frame) jc)
      (.pack frame)
      (.setVisible frame true)
      (redraw-screen state) 
      frame)))

(defn new-state
  "Create a brand new game state."
  ([]
    (let [state {:game (atom (world/new-game))
                 :console (new-console)
                 :frame (new-frame)
                 :event-handler (atom nil)}]
      (reset! (:event-handler state) (make-main-handler state))
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