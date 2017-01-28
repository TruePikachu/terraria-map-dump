(defpackage :terraria-map-dump.game-info
  (:use :common-lisp :terraria-map-dump.map-header)
  (:export :rgba-color :%rgba :copy-rgba :rgba-r :rgba-g :rgba-b :rgba-a :rgba-color-p
           :game-info :copy-game-info :game-info-p :game-info-unknown-color
           :game-info-tile-color :game-info-tile-name :game-info-wall-color
           :game-info-water-color :game-info-lava-color :game-info-honey-color
           :game-info-sky-color :game-info-dirt-color :game-info-rock-color
           :game-info-hell-color :get-game-info))
(in-package :terraria-map-dump.game-info)

(deftype rgba-color () '(vector (unsigned-byte 8) 4))
(defstruct (rgba-color
             (:type (vector (unsigned-byte 8)))
             (:constructor %rgba (r g b &optional (a 255)))
             (:copier copy-rgba)
             (:conc-name rgba-))
  (r 255 :type (unsigned-byte 8))
  (g 255 :type (unsigned-byte 8))
  (b 255 :type (unsigned-byte 8))
  (a 255 :type (unsigned-byte 8)))

(defstruct game-info
  (unknown-color (%rgba 0 0 0) :type rgba-color :read-only t)
  (tile-color #() :type (vector (vector rgba-color)) :read-only t)
  (tile-name #() :type (vector keyword) :read-only t)
  (wall-color #() :type (vector (vector rgba-color)) :read-only t)
  (water-color (%rgba 0 0 0) :type rgba-color :read-only t)
  (lava-color (%rgba 0 0 0) :type rgba-color :read-only t)
  (honey-color (%rgba 0 0 0) :type rgba-color :read-only t)
  (sky-color #() :type (vector rgba-color) :read-only t)
  (dirt-color #() :type (vector rgba-color) :read-only t)
  (rock-color #() :type (vector rgba-color) :read-only t)
  (hell-color (%rgba 0 0 0) :type rgba-color :read-only t))

(defun get-game-info (map-header)
  (destructuring-bind
    (keywords offsets colors) (with-open-file
                                (in (make-pathname
                                      :name (format nil "~A"
                                                    (map-header-game-release
                                                      map-header))
                                      :type "sexp"
                                      :defaults (merge-pathnames
                                                  #P"game-info/"
                                                  (asdf:system-source-directory
                                                    :terraria-map-dump)))
                                    :if-does-not-exist :error)
                                (read in))
    (declare (ignore offsets))
    (labels ((next-color () (destructuring-bind (r g b a) (pop colors)
                              (%rgba r g b a)))
             (read-variations
               (variation-count)
               (coerce
                 (loop for i from 0 below variation-count
                       collect (next-color)) '(vector rgba-color)))
             (read-options
               (option-counts)
               (coerce
                 (loop for count across option-counts
                       collect (read-variations count))
                 '(vector (vector rgba-color)))))
      (make-game-info
        :unknown-color (next-color)
        :tile-color (read-options (map-header-tile-option-counts map-header))
        :tile-name (coerce keywords '(vector keyword))
        :wall-color (read-options (map-header-wall-option-counts map-header))
        :water-color (next-color)
        :lava-color (next-color)
        :honey-color (next-color)
        :sky-color (read-variations (map-header-sky-type-count map-header))
        :dirt-color (read-variations (map-header-dirt-type-count map-header))
        :rock-color (read-variations (map-header-rock-type-count map-header))
        :hell-color (next-color)))))
