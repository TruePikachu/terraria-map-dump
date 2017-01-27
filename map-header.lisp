(defpackage :terraria-map-dump.map-header
  (:use :common-lisp :terraria-map-dump.binary-reader)
  (:export :copy-map-header :map-header-dirt-type-count
           :map-header-file-favorite-p :map-header-file-revision
           :map-header-fluid-type-count :map-header-game-release :map-header-p
           :map-header-rock-type-count :map-header-sky-type-count
           :map-header-tile-option-counts :map-header-wall-option-counts
           :map-header-world-height :map-header-world-id :map-header-world-name
           :map-header-world-width :read-map-header))
(in-package :terraria-map-dump.map-header)

(defstruct map-header
  "All the information from a map file, excluding the actual map"
  (game-release 0 :type (unsigned-byte 32) :read-only t)
  (file-revision 0 :type (unsigned-byte 32) :read-only t)
  (file-favorite-p nil :type boolean :read-only t)
  (world-name "" :type string :read-only t)
  (world-id 0 :type (unsigned-byte 32) :read-only t)
  (world-height 0 :type (unsigned-byte 32) :read-only t)
  (world-width 0 :type (unsigned-byte 32) :read-only t)
  (tile-option-counts #() :type (vector (unsigned-byte 8)) :read-only t)
  (wall-option-counts #() :type (vector (unsigned-byte 8)) :read-only t)
  (fluid-type-count 0 :type (unsigned-byte 16) :read-only t)
  (sky-type-count 0 :type (unsigned-byte 16) :read-only t)
  (dirt-type-count 0 :type (unsigned-byte 16) :read-only t)
  (rock-type-count 0 :type (unsigned-byte 16) :read-only t))

(defun read-map-header (stream)
  "Read a MAP-HEADER structure from STREAM"
  (let* ((game-release (read-le stream 4))
         (magic-code (read-le stream 8))
         (file-revision (read-le stream 4))
         (file-favorite-p (plusp (read-le stream 8)))
         (world-name (read-string stream))
         (world-id (read-le stream 4))
         (world-height (read-le stream 4))
         (world-width (read-le stream 4))
         (tile-type-count (read-le stream 2))
         (wall-type-count (read-le stream 2))
         (fluid-type-count (read-le stream 2))
         (sky-type-count (read-le stream 2))
         (dirt-type-count (read-le stream 2))
         (rock-type-count (read-le stream 2))
         (plusp-tile-option-counts (read-bit-vector stream tile-type-count))
         (plusp-wall-option-counts (read-bit-vector stream wall-type-count))
         (tile-option-counts (make-array (list tile-type-count)
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 1))
         (wall-option-counts (make-array (list wall-type-count)
                                         :element-type '(unsigned-byte 8)
                                         :initial-element 1)))
    (loop for i upfrom 0
          for plusp across plusp-tile-option-counts
          when (plusp plusp) do (setf (elt tile-option-counts i)
                              (read-le stream 1)))
    (loop for i upfrom 0
          for plusp across plusp-wall-option-counts
          when (plusp plusp) do (setf (elt wall-option-counts i)
                              (read-le stream 1)))
    (make-map-header
      :game-release game-release
      :file-revision file-revision
      :file-favorite-p file-favorite-p
      :world-name world-name
      :world-id world-id
      :world-height world-height
      :world-width world-width
      :tile-option-counts tile-option-counts
      :wall-option-counts wall-option-counts
      :fluid-type-count fluid-type-count
      :sky-type-count sky-type-count
      :dirt-type-count dirt-type-count
      :rock-type-count rock-type-count)))
