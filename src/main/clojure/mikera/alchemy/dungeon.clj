(ns mikera.alchemy.dungeon
  (:use mikera.orculje.core)
  (:require [mikera.alchemy.lib :as lib])
  (:require [mikera.orculje.mapmaker :as mm]))

(defn maybe-place-thing [game l1 l2 t]
  (or (mm/place-thing game l1 l2 t)
      game))

(defn generate
  "Main dungeon generation algorithm"
  [game]
  (as-> game game
    (mm/fill-block game (loc -4 -4 0) (loc 12 4 0) (lib/create game "wall"))
    (mm/fill-block game (loc -3 -3 0) (loc 3 3 0) (lib/create game "floor"))
    (mm/fill-block game (loc 4 0 0) (loc 4 0 0) (lib/create game "floor"))
    (mm/fill-block game (loc 5 -3 0) (loc 11 3 0) (lib/create game "floor"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-apparatus]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "rat"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-reptile]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-potion]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-potion]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-potion]"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "magic mushroom"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "slime mould"))
    (maybe-place-thing game (loc -3 -3 0) (loc 11 3 0) (lib/create game "[:is-food]"))
    (maybe-place-thing game (loc 4 0 0) (loc 4 0 0) (lib/create game "door"))))