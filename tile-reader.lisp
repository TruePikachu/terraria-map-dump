(defpackage :terraria-map-dump.tile-reader
  (:use :common-lisp :deflate :flex :terraria-map-dump.binary-reader
        :terraria-map-dump.map-header :terraria-map-dump.map-tile)
  (:export :read-map-data))
(in-package :terraria-map-dump.tile-reader)

(defmacro with-inflated-stream ((inflated deflated) &body body)
  "Execute BODY with decompressed data from DEFLATED available from INFLATED"
  (let ((intermediate-stream-name (gensym)))
    `(with-input-from-sequence
       (,inflated
         (with-output-to-sequence
           (,intermediate-stream-name)
           (inflate-stream ,deflated ,intermediate-stream-name)))
       ,@body)))

(defun make-base-mapper (map-header)
  (flet ((make-mapper
           (option-counts)
           (coerce
             (loop for id upfrom 0
                   for option-count across option-counts
                   append (loop for option from 0 below option-count
                                collect (cons id option))) 'vector)))
    (cons (make-mapper (map-header-tile-option-counts map-header))
          (make-mapper (map-header-wall-option-counts map-header)))))

(defun read-tile-chunk (stream base-mapper)
  (flet ((next-byte () (read-le stream 1))
         (next-word () (read-le stream 2)))
    (let* ((flags (next-byte))
           (class (ldb (byte 3 1) flags))
           (color-id (if (logbitp 0 flags) (next-byte) 0))
           (type (case class
                   ((#b001 #b010 #b111) (if (logbitp 4 flags)
                                          (next-word)
                                          (next-byte)))
                   (otherwise 0)))
           (light (if (logbitp 5 flags) (next-byte)
                    (if (plusp class) 255 0)))
           (rle-count (cond
                        ((logbitp 6 flags) (next-byte))
                        ((logbitp 7 flags) (next-word))
                        (t 0)))
           (base-tile (case class
                        (#b000 (make-unknown-tile :color color-id
                                                  :light light))
                        (#b001 (destructuring-bind
                                 (id . var) (elt (car base-mapper) type)
                                 (make-world-tile :color color-id
                                                  :light light
                                                  :id id
                                                  :variation var)))
                        (#b010 (destructuring-bind
                                 (id . var) (elt (cdr base-mapper) type)
                                 (make-wall-tile :color color-id
                                                 :light light
                                                 :id id
                                                 :variation var)))
                        (#b011 (make-water-tile :color color-id
                                                :light light))
                        (#b100 (make-lava-tile :color color-id
                                               :light light))
                        (#b101 (make-honey-tile :color color-id
                                                :light light))
                        (#b110 (make-sky-tile :color color-id
                                              :light light
                                              :variation type))
                        (#b111 (make-ground-tile :color color-id
                                                 :light light
                                                 :variation type)))))
      (if (and (plusp rle-count) (logbitp 5 flags) (plusp class))
        (loop for i from 0 to rle-count
              for l = light then (next-byte)
              collect (copy-tile-with-light base-tile l))
        (loop for i from 0 to rle-count
              collect base-tile)))))

(defun read-map-data (filename)
  "Read the map at FILENAME, return: header, map tile array"
  (with-open-file (stream filename
                          :element-type '(unsigned-byte 8))
    (let* ((header (read-map-header stream))
           (mapper (make-base-mapper header))
           (result (make-array (list (map-header-world-height header)
                                     (map-header-world-width header))
                               :initial-element (make-unknown-tile)
                               :element-type 'map-tile)))
      (with-inflated-stream
        (data stream)
        (dotimes (y (map-header-world-height header))
          (let ((x 0))
            (loop
              (loop for tile in (read-tile-chunk data mapper)
                    do (setf (aref result y x) tile) (incf x))
              (assert (<= x (map-header-world-width header)))
              (when (= x (map-header-world-width header))
                (return)))))
        (assert (null (read-byte data nil))))
      (values header result))))
