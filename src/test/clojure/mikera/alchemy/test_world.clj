(ns mikera.alchemy.test-world
  (:use mikera.alchemy.world)
    (:require [mikera.alchemy.lib :as lib])
  (:use mikera.orculje.core)
  (:use clojure.test))

(deftest test-game
  (let [game (new-game)]
    (assert (not (seq (things game 0 0 0))))
    (assert (some 
              #(= (:name %) "you")
              (lib/all-things game)))))