(defpackage :terraria-map-dump.render-map
  (:use :common-lisp :terraria-map-dump.game-info :terraria-map-dump.map-header :terraria-map-dump.map-tile :terraria-map-dump.tile-reader :zpng)
  (:export :render-map-png :simple-render-map-png))
(in-package :terraria-map-dump.render-map)

(defun tile-base-color (tile info y h)
  (etypecase tile
    (unknown-tile (game-info-unknown-color info))
    (world-tile (elt (elt (game-info-tile-color info)
                          (tile-id tile))
                     (tile-variation tile)))
    (wall-tile (elt (elt (game-info-wall-color info)
                         (tile-id tile))
                    (tile-variation tile)))
    (water-tile (game-info-water-color info))
    (lava-tile (game-info-lava-color info))
    (honey-tile (game-info-honey-color info))
    (sky-tile (if (< y (/ h 2)) ; TODO Accuracy
                (elt (game-info-sky-color info) 128)
                (game-info-hell-color info)))
    (ground-tile (elt (game-info-rock-color info) ; TODO Accuracy
                      (tile-variation tile)))))

(defun paint-color (color)
  (case color
    ((1 13) (%rgba 255 0 0))
    ((2 14) (%rgba 255 127 0))
    ((3 15) (%rgba 255 255 0))
    ((4 16) (%rgba 127 255 0))
    ((5 17) (%rgba 0 255 0))
    ((6 18) (%rgba 0 255 127))
    ((7 19) (%rgba 0 255 255))
    ((8 20) (%rgba 0 127 255))
    ((9 21) (%rgba 0 0 255))
    ((10 22) (%rgba 127 0 255))
    ((11 23) (%rgba 255 0 255))
    ((12 24) (%rgba 255 0 127))
    (25 (%rgba 75 75 75))
    (26 (%rgba 255 255 255))
    (27 (%rgba 175 175 175))
    (28 (%rgba 255 178 125))
    (29 (%rgba 25 25 25))
    (30 (%rgba 200 200 200 150))
    (otherwise (%rgba 255 255 255))))

(defun tile-painted-color (tile info y h)
  (let* ((base-color (tile-base-color tile info y h))
         (paint-color (paint-color (tile-raw-color tile)))
         (brightest-value (max (rgba-r base-color)
                               (rgba-g base-color)
                               (rgba-b base-color))))
    (if (zerop (tile-raw-color tile))
      base-color
      (flet ((scale-color
               (color factor)
               (%rgba (round (* factor (rgba-r color)))
                      (round (* factor (rgba-g color)))
                      (round (* factor (rgba-b color)))
                      (rgba-a base-color))))
        (case (tile-raw-color tile)
          (29 (scale-color paint-color
                           (* 3/10
                              (/ (if (= (rgba-b base-color) brightest-value)
                                   (max (rgba-r base-color) (rgba-g base-color))
                                   (rgba-b base-color)) 255))))
          (30 (let ((inverted-color (%rgba (- 255 (rgba-r base-color))
                                           (- 255 (rgba-g base-color))
                                           (- 255 (rgba-b base-color))
                                           (rgba-a base-color))))
                (if (typep tile 'wall-tile)
                  (scale-color inverted-color 1/2)
                  inverted-color)))
          (otherwise (scale-color paint-color
                                  (/ brightest-value 255))))))))

(defun tile-lit-color (tile info y h)
  (let ((unlit-color (tile-painted-color tile info y h))
        (light-amount (/ (tile-light-level tile) 255)))
    (if (= light-amount 1)
      unlit-color
      (%rgba (round (* (rgba-r unlit-color) light-amount))
             (round (* (rgba-g unlit-color) light-amount))
             (round (* (rgba-b unlit-color) light-amount))
             (rgba-a unlit-color)))))

(defun render-map-png (header data out-stream)
  (let ((image (make-instance 'pixel-streamed-png
                              :color-type :truecolor-alpha
                              :width (map-header-world-width header)
                              :height (map-header-world-height header)))
        (info (get-game-info header)))
    (zpng:start-png image out-stream)
    (dotimes (y (map-header-world-height header))
      (dotimes (x (map-header-world-width header))
        (let ((color (tile-lit-color (aref data y x) info y
                                     (map-header-world-height header))))
          (write-pixel
            (list (rgba-r color) (rgba-g color)
                  (rgba-b color) (rgba-a color)) image))))
    (zpng:finish-png image))
  nil)

(defun simple-render-map-png (map-filename image-filename)
  (multiple-value-bind (header data) (read-map-data map-filename)
    (with-open-file (out image-filename
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (render-map-png header data out))))
