(defpackage :terraria-map-dump.render-png
  (:nicknames :tmapdump.render-png)
  (:use :common-lisp :tmapdump.color :tmapdump.map :tmapdump.tile :zpng)
  (:export :render-png))
(in-package :terraria-map-dump.render-png)

(defun biome-spread-rgba (tile y h)
  (let ((sets (when (or (typep tile 'block-tile)
                        (typep tile 'wall-tile))
                (tile-sets tile))))
    (if (or (find :corrupt sets)
            (find :crimson sets)
            (find :hallow sets))
      (tile-raw-rgba tile y h)
      (let ((color (grayscale-color
                     (tile-rgba tile y h))))
        (%rgba (color-r color) (color-g color) (color-b color)
               (round (color-a color) 2))))))

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
                         (:biome-spread #'biome-spread-rgba))))
        (zpng:start-png image stream)
        (dotimes (y (array-dimension data 0))
          (dotimes (x (array-dimension data 1))
            (let ((color (funcall color-fn (aref data y x) y (array-dimension data 0))))
              (write-pixel (coerce color 'list) image))))
        (zpng:finish-png image))))
  file-or-stream)
