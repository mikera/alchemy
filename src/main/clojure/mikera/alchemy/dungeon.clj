(ns mikera.alchemy.dungeon
  (:use mikera.orculje.core)
  (:require [mikera.alchemy.lib :as lib])
  (:require [mikera.orculje.mapmaker :as mm]))

(defn generate
  "Main dungeon generation algorithm"
  [game]
  (as-> game game
    (mm/fill-block game (loc -4 -4 0) (loc 4 4 0) (lib/create game "wall"))
    (mm/fill-block game (loc -3 -3 0) (loc 3 3 0) (lib/create game "floor"))))