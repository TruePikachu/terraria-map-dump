# terraria-map-dump
Terraria Map Dumper

## Overview
`terraria-map-dump` aims to be a Common Lisp system able to access information from a Terraria character's minimap save files. The core module will allow access to a loaded minimap's saved tile data, and supplementary packages will allow useful operations to be performed (such as locating all unmined ore, statues, or chests that have been seen by the character).

## Example Usage
```common-lisp
(defvar *map* (tmapdump:read-map #P"world.map"))  => *MAP*
(tmapdump:render-png *map* #P"map.png")  => #P"map.png"
(tmapdump:render-png *map* #P"biomes.png" #'biome-spread-rgba)  => #P"biomes.png"
```

## Map Render Types
Functions which render a map to a file (such as `#'RENDER-PNG`) take an optional parameter `COLOR-FN` which specifies how the map's colors should be decided. Some possible values are:

* `#'TILE-RGBA` - Default map generation, aims to be identical to in-game minimap.
* `#'TILE-RAW-RGBA` - Renders tiles at full brightness, ignoring light levels.
* `#'DULL-TILE-RGBA` - Renders tiles in grayscale and at 50% of their normal transparency.
* `#'BIOME-SPREAD-RGBA` - Corruption/Crimson/Hallow biome visualization; tiles belonging to the listed groups are rendered with `#'TILE-RAW-RGBA`, and other tiles are rendered with `#'DULL-TILE-RGBA`.
* `#'TREASURE-RGBA` - Many treasure-related tiles are rendered with `#'TILE-RAW-RGBA`, and other tiles are rendered with `#'DULL-TILE-RGBA`.

The actual function passed should take three arguments (a `TILE`, an integer representing the map row number, and an `ELEVATION-PROFILE`), and return a `COLOR`.

## Game Info Search Order
In order to do anything useful with map data, a "`game-info`" file is required. These are keyed by game release, and are located in one of the following locations:

* Current directory, as e.g. `game-info-1234.sexp`
* Internally to the system, at system compile-time (these are sourced from the `game-info` directory)
* System's `game-info` directory (i.e. where the above are sourced from)

These `game-info` files are generally created using the `dump-terraria-info.ps1` Powershell script.
