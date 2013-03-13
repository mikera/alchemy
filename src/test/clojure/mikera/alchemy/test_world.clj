(ns mikera.alchemy.test-world
  (:use mikera.alchemy.world)
  (:use mikera.alchemy.engine)
    (:require [mikera.alchemy.lib :as lib])
  (:use mikera.orculje.core)
  (:use clojure.test))

(deftest test-game
  (let [game (new-game)]
    (is (not (seq (get-things game 1 0 0))))
    (is (some 
              #(= (:name %) "you")
              (lib/all-library-things game)))
    (is (= (hero-location game) (location game (get-thing game (hero game)))))))