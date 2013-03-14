(ns mikera.alchemy.dungeon
  (:use mikera.orculje.core)
  (:require [mikera.alchemy.lib :as lib])
  (:require [mikera.orculje.mapmaker :as mm]))

(defn generate
  "Main dungeon generation algorithm"
  [game]
  (as-> game game
    (mm/fill-block game (loc -4 -4 0) (loc 12 4 0) (lib/create game "wall"))
    (mm/fill-block game (loc -3 -3 0) (loc 3 3 0) (lib/create game "floor"))
    (mm/fill-block game (loc 4 0 0) (loc 4 0 0) (lib/create game "floor"))
    (mm/fill-block game (loc 5 -3 0) (loc 11 3 0) (lib/create game "floor"))
    (mm/place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "alchemy bench"))
    (mm/place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "rat"))
    (mm/place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-reptile]"))
    (mm/place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "potion of healing"))
    (mm/place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "potion of healing"))
    (mm/place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "potion of healing"))
    (mm/place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "magic mushroom"))
    (mm/place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "slime mould"))
    (mm/place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "slime mould"))
    (mm/place-thing game (loc 4 0 0) (loc 4 0 0) (lib/create game "door"))))