(defpackage :terraria-map-dump.render-png
  (:nicknames :tmapdump.render-png)
  (:use :common-lisp :tmapdump.color :tmapdump.map :tmapdump.tile :zpng)
  (:export :render-png))
(in-package :terraria-map-dump.render-png)

(defun dull-tile-rgba (tile y h)
  "Renders tiles grayscale at 50% normal transparency"
  (let ((color (grayscale-color
                 (tile-rgba tile y h))))
    (%rgba (color-r color) (color-g color) (color-b color)
           (round (color-a color) 2))))

(defun biome-spread-rgba (tile y h)
  (let ((sets (when (or (typep tile 'block-tile)
                        (typep tile 'wall-tile))
                (tile-sets tile))))
    (if (or (find :corrupt sets)
            (find :crimson sets)
            (find :hallow sets))
      (tile-raw-rgba tile y h)
      (dull-tile-rgba tile y h))))

(defun treasure-rgba (tile y h)
  (if (and (typep tile 'block-tile)
           (find (tile-id tile) (list 6 ; Iron Ore
                                      7 ; Copper Ore
                                      8 ; Gold Ore
                                      9 ; Silver Ore
                                      12 ; Crystal Heart
                                      21 ; Chest
                                      22 ; Demonite Ore
                                      28 ; Pot
                                      37 ; Meteorite
                                      58 ; Hellstone
                                      63 ; Sapphire
                                      64 ; Ruby
                                      65 ; Emerald
                                      66 ; Topaz
                                      67 ; Amethyst
                                      68 ; Diamond
                                      107 ; Cobalt Ore
                                      108 ; Mythril Ore
                                      111 ; Adamantite Ore
                                      166 ; Tin Ore
                                      167 ; Lead Ore
                                      168 ; Tungsten Ore
                                      169 ; Platinum Ore
                                      178 ; Gems?
                                      185 ; Small Pile
                                      186 ; Large Pile
                                      187 ; Large Pile
                                      204 ; Crimtane Ore
                                      211 ; Chlorophyte Ore
                                      221 ; Palladium Ore
                                      222 ; Orichalcum Ore
                                      223 ; Titanium Ore
                                      236 ; Life Fruit
                                      239 ; Bars
                                      330 ; Copper Coin Pile
                                      331 ; Silver Coin Pile
                                      332 ; Gold Coin Pile
                                      333))); Platinum Coin Pile
    (tile-raw-rgba tile y h)
    (dull-tile-rgba tile y h)))


(defun render-png (map file-or-stream &optional (map-type :default))
  "Render MAP to FILE-OR-STREAM"
  (etypecase file-or-stream
    (pathname (with-open-file (out file-or-stream
                                   :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
                (render-png map out map-type)))
    (stream
      (let* ((stream file-or-stream)
             (data (minimap-data map))
             (image (make-instance 'pixel-streamed-png
                                   :color-type :truecolor-alpha
                                   :width (array-dimension data 1)
                                   :height (array-dimension data 0)))
             (color-fn (ecase map-type
                         (:default #'tile-rgba)
                         (:biome-spread #'biome-spread-rgba)
                         (:treasure #'treasure-rgba))))
        (zpng:start-png image stream)
        (dotimes (y (array-dimension data 0))
          (dotimes (x (array-dimension data 1))
            (let ((color (funcall color-fn (aref data y x) y (array-dimension data 0))))
              (write-pixel (coerce color 'list) image))))
        (zpng:finish-png image))))
  file-or-stream)
