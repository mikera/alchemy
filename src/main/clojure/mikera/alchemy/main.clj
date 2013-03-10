(ns mikera.alchemy.main
  (:import [javax.swing JFrame JComponent])
  (:import [java.awt Font])
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

(defn launch 
  "Launch the game with an initial game state. Can be called from REPL."
  ([state]
    (let [^JFrame frame (:frame state)
          ^JConsole jc (:console state)]
      (.add (.getContentPane frame) jc)
      (.pack frame)
      (.setVisible frame true)
      (.write jc "Alchemy Lives!") 
      frame)))

(defn new-state
  "Create a brand new game state."
  ([]
    (let [state {:game (atom nil)
                 :console (new-console)
                 :frame (new-frame)
                 :event-handler (atom nil)}]
      state)))

;; a state for the world
(def s (new-state))


(defn main 
  "Main entry point to the demo, called directly from Java main() method in DemoApp"
  ([]
    (let [^JFrame frame (launch s)]
      (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE))))