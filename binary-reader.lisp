(defpackage :terraria-map-dump.binary-reader
  (:use :common-lisp)
  (:export :read-array :read-bit-vector :read-le :read-string))
(in-package :terraria-map-dump.binary-reader)

(defun read-le (stream length)
  "Read a LENGTH-byte little-endian unsigned integer from STREAM"
  (loop for i from 0 below length
        summing (ash (read-byte stream) (* i 8))))

(defun read-array (stream dimension element-fn)
  "Read a DIMENSION-element array from STREAM as a list; each element is read with (ELEMENT-FN STREAM)"
  (loop for i from 0 below dimension
        collect (funcall element-fn stream)))

(defun read-string (stream)
  "Read a byte-length-prefixed 8-bit-character string from STREAM"
  (coerce (read-array stream
                      (read-le stream 1)
                      (lambda (s)
                        (code-char (read-le s 1))))
          'string))

(defun read-bit-vector (stream length)
  "Read a BIT-VECTOR of LENGTH bits from STREAM"
  (coerce
    (let (buffer)
      (loop for i from 0 below length
            for bit = (mod i 8)
            when (zerop bit) do (setf buffer (read-le stream 1)) end
            collect (if (logbitp bit buffer)
                      1 0))) 'bit-vector))
