(ns mikera.alchemy.test-main
  (:use clojure.test)
  (:use mikera.alchemy.main))

(deftest test-new-state
  (let [s (new-state)]
    (is @(:game s))))

