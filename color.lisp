(defpackage :terraria-map-dump.color
  (:nicknames :tmapdump.color)
  (:use :common-lisp)
  (:export :%rgba :color :color-a :color-b :color-g :color-p :color-r
           :copy-color :invert-color))
(in-package :terraria-map-dump.color)

(deftype color () '(vector (unsigned-byte 8) 4))
(defstruct (color
             (:type (vector (unsigned-byte 8)))
             (:constructor %rgba (r g b &optional a)))
  (r 255 :type (unsigned-byte 8))
  (g 255 :type (unsigned-byte 8))
  (b 255 :type (unsigned-byte 8))
  (a 255 :type (unsigned-byte 8)))

(defun invert-color (color)
  "Return the inverse of COLOR"
  (%rgba (- 255 (color-r color))
         (- 255 (color-g color))
         (- 255 (color-b color))
         (color-a color)))

