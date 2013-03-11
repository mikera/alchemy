(ns mikera.alchemy.test-world
  (:use mikera.alchemy.world)
  (:use mikera.orculje.core)
  (:use clojure.test))

(deftest test-game
  (let [g (new-game)]
    (assert (not (seq (things g 0 0 0))))))