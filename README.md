# terraria-map-dump
Terraria Map Dumper

## Overview
`terraria-map-dump` aims to be a Common Lisp system able to access information from a Terraria character's minimap save files. The core module will allow access to a loaded minimap's saved tile data, and supplementary packages will allow useful operations to be performed (such as locating all unmined ore, statues, or chests that have been seen by the character).

## Example Usage
```common-lisp
(defvar *map* (tmapdump:read-map #P"world.map"))  => *MAP*
(tmapdump:render-png *map* #P"map.png")  => #P"map.png"
```
