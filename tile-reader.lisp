(defpackage :terraria-map-dump.tile-reader
  (:use :common-lisp :deflate :flex :terraria-map-dump.binary-reader
        :terraria-map-dump.map-header))
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
