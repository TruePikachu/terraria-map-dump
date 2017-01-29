# terraria-map-dump
Terraria Map Dumper

## Overview
`terraria-map-dump` aims to be a Common Lisp system able to access information from a Terraria character's minimap save files. The core module will allow access to a loaded minimap's saved tile data, and supplementary packages will allow useful operations to be performed (such as locating all unmined ore, statues, or chests that have been seen by the character).

## Example Usage
```common-lisp
(defvar *map* (tmapdump:read-map #P"world.map"))  => *MAP*
(tmapdump:render-png *map* #P"map.png")  => #P"map.png"
(tmapdump:render-png *map* #P"biomes.png" :biome-spread)  => #P"biomes.png"
```

## Map Render Types
Functions which render a map to a file (such as `#'RENDER-PNG`) take an optional parameter `MAP-TYPE` which specifies what kind of map should be generated:

* `:DEFAULT` - Default map generation, aims to be identical to in-game minimap
* `:BIOME-SPREAD` - Corruption/Crimson/Hallow biome visualization; tiles belonging to the listed groups are rendered as normal (though at full brightness), tiles not in those groups are rendered in grayscale at 50% their normal transparency.
* `:TREASURE` - Similar to above, but a number of specific blocks are at full brightness, and everything else is dulled out
