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
  (labels ((parse-color
             (color)
             (destructuring-bind
               (r g b a) color
               (%rgba r g b a)))
           (parse-color-list
             (color-list)
             (coerce (mapcar #'parse-color color-list)
                     '(vector rgba-color))))
    (let* ((root-game-info
             (with-open-file
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
               (read in)))
           (fluid-colors (parse-color-list
                           (cdr (assoc :liquids root-game-info))))
           (tile-info (cdr (assoc :tiles root-game-info)))
           (wall-info (cdr (assoc :walls root-game-info))))
      (make-game-info
        :unknown-color (parse-color (cdr (assoc :empty root-game-info)))
        :tile-color (coerce (mapcar (lambda (v)
                                      (parse-color-list
                                        (third v))) tile-info)
                            '(vector rgba-color))
        :tile-name (coerce (mapcar #'first tile-info) '(vector keyword))
        :wall-color (coerce (mapcar (lambda (v)
                                      (parse-color-list
                                        (third v))) wall-info)
                            '(vector rgba-color))
        :water-color (elt fluid-colors 0)
        :lava-color (elt fluid-colors 1)
        :honey-color (elt fluid-colors 2)
        :sky-color (parse-color-list (cdr (assoc :sky root-game-info)))
        :dirt-color (parse-color-list (cdr (assoc :dirt root-game-info)))
        :rock-color (parse-color-list (cdr (assoc :rock root-game-info)))
        :hell-color (parse-color (cdr (assoc :hell root-game-info)))))))
