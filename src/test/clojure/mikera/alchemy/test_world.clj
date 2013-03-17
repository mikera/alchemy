(ns mikera.alchemy.test-world
  (:use mikera.alchemy.world)
  (:require [mikera.alchemy.engine :as engine])
  (:require [mikera.alchemy.lib :as lib])
  (:use mikera.orculje.core)
  (:use clojure.test))

(deftest test-game
  (let [game (new-game)]
    (is (some 
              #(= (:name %) "you")
              (lib/all-library-things game)))
    (is (= (engine/hero-location game) (location game (get-thing game (engine/hero game)))))))