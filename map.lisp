(defpackage :terraria-map-dump.map
  (:nicknames :tmapdump.map)
  (:use :common-lisp :deflate :flex :tmapdump.binary-reader :tmapdump.tile))
(in-package :terraria-map-dump.map)

(defstruct minimap
  "Minimap information"
  (game-release 0 :type (unsigned-byte 32) :read-only t)
  (file-revision 0 :type (unsigned-byte 32) :read-only t)
  (world-name "" :type string :read-only t)
  (world-id 0 :type (unsigned-byte 32) :read-only t)
  (data #2A() :type (array tile 2) :read-only t))

(defmacro with-inflated-stream ((inflated deflated) &body body)
  "Execute BODY with decompressed data from DEFLATED available from INFLATED"
  (let ((intermediate-stream-name (gensym)))
    `(with-input-from-sequence
       (,inflated
         (with-output-to-sequence
           (,intermediate-stream-name)
           (inflate-stream ,deflated ,intermediate-stream-name)))
       ,@body)))

(defun read-map-data (stream h w)
  (let ((result (make-array (list h w)
                            :initial-element (make-empty-tile)
                            :element-type 'tile)))
    (with-inflated-stream
      (data stream)
      (dotimes (y h)
        (let ((x 0))
          (loop
            (loop for tile in (read-tiles data)
                  do (setf (aref result y x) tile) (incf x))
            (assert (<= x w))
            (when (= x w) (return)))))
      (assert (null (read-byte data nil))))
    result))

(defun read-map (stream-or-filename &optional (read-game-info-p t))
  "Read a MINIMAP from STREAM-OR-FILENAME"
  (etypecase stream-or-filename
    (pathname (with-open-file (in stream-or-filename
                                  :if-does-not-exist :error
                                  :element-type '(unsigned-byte 8))
                (read-map in read-game-info-p)))
    (stream
      (let* ((stream stream-or-filename)
             (game-release (read-le stream 4))
             (magic-code (read-le stream 8))
             (file-revision (read-le stream 4))
             (file-favorite-p (plusp (read-le stream 8)))
             (world-name (read-string stream))
             (world-id (read-le stream 4))
             (height (read-le stream 4))
             (width (read-le stream 4))
             (tile-tc (read-le stream 2))
             (wall-tc (read-le stream 2))
             (fluid-tc (read-le stream 2))
             (sky-tc (read-le stream 2))
             (dirt-tc (read-le stream 2))
             (rock-tc (read-le stream 2))
             (not-one-tile-oc-p (read-bit-vector stream tile-tc))
             (not-one-wall-oc-p (read-bit-vector stream wall-tc))
             (tile-oc (make-array (list tile-tc)
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 1))
             (wall-oc (make-array (list wall-tc)
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 1)))
        ; TODO Validate header
        (declare (ignore magic-code file-favorite-p fluid-tc sky-tc dirt-tc rock-tc))
        (loop for i upfrom 0
              for not-one-p across not-one-tile-oc-p
              when (plusp not-one-p) do (setf (elt tile-oc i)
                                              (read-le stream 1)))
        (loop for i upfrom 0
              for not-one-p across not-one-wall-oc-p
              when (plusp not-one-p) do (setf (elt wall-oc i)
                                              (read-le stream 1)))
        (when read-game-info-p
          (set-game-info game-release))
        (make-minimap
          :game-release game-release
          :file-revision file-revision
          :world-name world-name
          :world-id world-id
          :data (read-map-data stream height width))))))
