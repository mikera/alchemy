(ns mikera.alchemy.test-main
  (:use clojure.test)
  (:use mikera.alchemy.main)
  (:require [mikera.alchemy.world :as world])
  (:require [mikera.alchemy.engine :as engine]))

; Doesn't work with Travis CI because of headless exception
;(deftest test-new-state
;  (let [s (new-state)
;        game @(:game s)]
;    (is game))))
;
