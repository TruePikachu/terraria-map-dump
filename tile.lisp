(defpackage :terraria-map-dump.tile
  (:nicknames :tmapdump.tile)
  (:use :common-lisp :tmapdump.binary-reader :tmapdump.color)
  (:export :block-tile :elevation-profile :elevation-profile-rock-layer
           :elevation-profile-world-surface :empty-tile :fluid-tile :ground-tile
           :honey-tile :lava-tile :make-block-tile :make-elevation-profile
           :make-empty-tile :make-ground-tile :make-honey-tile :make-lava-tile
           :make-sky-tile :make-wall-tile :make-water-tile :read-tiles
           :set-game-info :sky-tile :tile :tile-base-color :tile-id
           :tile-light-level :tile-name :tile-paint-id :tile-raw-rgba :tile-rgba
           :tile-sets :tile-variation :wall-tile :water-tile))
(in-package :terraria-map-dump.tile)

(defgeneric copy-tile-with-light (tile new-light))
(defgeneric tile-base-color (tile &optional y profile)
  (:documentation
    "Unlit, unpainted COLOR of TILE"))
(defgeneric tile-id (tile)
  (:documentation
    "ID number of TILE"))
(defgeneric tile-light-level (tile)
  (:documentation
    "Light level in TILE"))
(defgeneric tile-name (tile)
  (:documentation
    "KEYWORD name of TILE"))
(defgeneric tile-paint-id (tile)
  (:documentation
    "ID of the paint on TILE; 0 if unpainted"))
(defgeneric tile-sets (tile)
  (:documentation
    "LIST of KEYWORD set names that TILE belongs to"))
(defgeneric tile-variation (tile)
  (:documentation
    "Variation ID number of TILE"))

(defvar *dirt-colors*)
(defvar *empty-color*)
(defvar *block-id-mapper*)
(defvar *block-info*)
(defvar *fluid-colors*)
(defvar *hell-color*)
(defvar *rock-colors*)
(defvar *sky-colors*)
(defvar *wall-id-mapper*)
(defvar *wall-info*)

(defstruct elevation-profile
  (world-surface 0 :type (unsigned-byte 32))
  (rock-layer 0 :type (unsigned-byte 32)))

(defstruct infos
  (name nil :type keyword :read-only t)
  (sets nil :type list :read-only t)
  (colors nil :type (vector color) :read-only t))
(defstruct tile
  "Base structure for map tiles"
  (paint 0 :type (unsigned-byte 7) :read-only t)
  (light 255 :type (unsigned-byte 8) :read-only t))
(defmethod copy-tile-with-light ((this tile) light)
  (funcall (etypecase this
             (empty-tile #'make-empty-tile)
             (water-tile #'make-water-tile)
             (lava-tile #'make-lava-tile)
             (honey-tile #'make-honey-tile)
             (sky-tile #'make-sky-tile))
           :paint (tile-paint this)
           :light light))
(defmethod tile-light-level ((this tile))
  (tile-light this))
(defmethod tile-paint-id ((this tile))
  (tile-paint this))
(defstruct (empty-tile
             (:include tile
                       (light 0)))
  "Tile which has unknown content")
(defmethod tile-base-color ((this empty-tile) &optional y profile)
  (declare (ignore y profile))
  *empty-color*)
(defmethod tile-name ((this empty-tile))
  nil)
(defstruct (id-tile
             (:include tile))
  (id 0 :type (unsigned-byte 16) :read-only t)
  (choice 0 :type (unsigned-byte 16) :read-only t))
(defmethod copy-tile-with-light ((this id-tile) light)
  (funcall (etypecase this
             (block-tile #'make-block-tile)
             (wall-tile #'make-wall-tile))
           :paint (id-tile-paint this)
           :light light
           :id (id-tile-id this)
           :choice (id-tile-choice this)))
(defmethod tile-id ((this id-tile))
  (id-tile-id this))
(defmethod tile-variation ((this id-tile))
  (id-tile-choice this))
(defstruct (block-tile
             (:include id-tile))
  "Tile which contains a block")
(defmethod tile-base-color ((this block-tile) &optional y profile)
  (declare (ignore y profile))
  (elt (infos-colors (elt *block-info* (block-tile-id this)))
       (block-tile-choice this)))
(defmethod tile-name ((this block-tile))
  (infos-name (elt *block-info* (block-tile-id this))))
(defmethod tile-sets ((this block-tile))
  (infos-sets (elt *block-info* (block-tile-id this))))
(defstruct (wall-tile
             (:include id-tile))
  "Tile which contains a wall")
(defmethod tile-base-color ((this wall-tile) &optional y profile)
  (declare (ignore y profile))
  (elt (infos-colors (elt *wall-info* (wall-tile-id this)))
       (wall-tile-choice this)))
(defmethod tile-name ((this wall-tile))
  (infos-name (elt *wall-info* (wall-tile-id this))))
(defmethod tile-sets ((this wall-tile))
  (infos-sets (elt *wall-info* (wall-tile-id this))))
(defstruct (fluid-tile
             (:include tile))
  "Tile which contains a fluid")
(defstruct (water-tile
             (:include fluid-tile))
  "Tile which contains water")
(defmethod tile-name ((this water-tile))
  :water)
(defmethod tile-base-color ((this water-tile) &optional y profile)
  (declare (ignore y profile))
  (elt *fluid-colors* 0))
(defstruct (lava-tile
             (:include fluid-tile))
  "Tile which contains lava")
(defmethod tile-base-color ((this lava-tile) &optional y profile)
  (declare (ignore y profile))
  (elt *fluid-colors* 1))
(defmethod tile-name ((this lava-tile))
  :lava)
(defstruct (honey-tile
             (:include fluid-tile))
  "Tile which contains honey")
(defmethod tile-base-color ((this honey-tile) &optional y profile)
  (declare (ignore y profile))
  (elt *fluid-colors* 2))
(defmethod tile-name ((this honey-tile))
  :honey)
(defstruct (variant-tile
             (:include tile))
  (variation 0 :type (unsigned-byte 8) :read-only t))
(defmethod copy-tile-with-light ((this variant-tile) light)
  (funcall (etypecase this
             (ground-tile #'make-ground-tile))
           :paint (variant-tile-paint this)
           :light light
           :variation (variant-tile-variation this)))
(defmethod tile-variation ((this variant-tile))
  (variant-tile-variation this))
(defstruct (sky-tile
             (:include tile))
  "Tile which displays sky or hell")
(defmethod tile-base-color ((this sky-tile) &optional y profile)
  (if (and y profile)
    (if (< y (elevation-profile-rock-layer profile))
      (let ((color-count (length *sky-colors*)))
        (elt *sky-colors*
             (min (1- color-count)
                  (round (* color-count (/ y (elevation-profile-world-surface profile)))))))
      *hell-color*)
    (elt *sky-colors* 127)))
(defmethod tile-name ((this sky-tile))
  :sky-or-hell)
(defstruct (ground-tile
             (:include variant-tile))
  "Tile which displays an underground background")
(defmethod tile-base-color ((this ground-tile) &optional y profile)
  (elt (if (and y profile (< y (elevation-profile-rock-layer profile)))
         *dirt-colors*
         *rock-colors*) (ground-tile-variation this)))
(defmethod tile-name ((this ground-tile))
  :underground)

(defun read-tiles (stream)
  "Read a chunk of tiles from STREAM"
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
                        (#b000 (make-empty-tile :paint color-id
                                                :light light))
                        (#b001 (destructuring-bind
                                 (id . var) (elt *block-id-mapper* type)
                                 (make-block-tile :paint color-id
                                                  :light light
                                                  :id id
                                                  :choice var)))
                        (#b010 (destructuring-bind
                                 (id . var) (elt *wall-id-mapper* type)
                                 (make-wall-tile :paint color-id
                                                 :light light
                                                 :id id
                                                 :choice var)))
                        (#b011 (make-water-tile :paint color-id
                                                :light light))
                        (#b100 (make-lava-tile :paint color-id
                                               :light light))
                        (#b101 (make-honey-tile :paint color-id
                                                :light light))
                        (#b110 (make-sky-tile :paint color-id
                                              :light light))
                        (#b111 (make-ground-tile :paint color-id
                                                 :light light
                                                 :variation type)))))
      (if (and (plusp rle-count) (logbitp 5 flags) (plusp class))
        (cons base-tile
              (loop for i from 1 to rle-count
                    for l = (next-byte)
                    collect (copy-tile-with-light base-tile l)))
        (loop for i from 0 to rle-count
              collect base-tile)))))

(defun get-info (release)
  (labels ((use-file (file) (with-open-file (in file) (return-from get-info (read in))))
           (try-use-file (file) (when (probe-file file) (use-file file))))
    (try-use-file (format nil "./game-info-~D.sexp" release))
    (let ((builtin-info
            (macrolet
              ((foo
                 (release)
                 (let ((game-infos
                         (directory (merge-pathnames
                                      #P"game-info/*.sexp"
                                      (asdf:system-source-directory
                                        :terraria-map-dump)))))
                   `(case ,release
                      ,@(loop for release in game-infos
                              collect `(,(parse-integer
                                           (pathname-name release))
                                         (quote ,(with-open-file
                                                   (in release)
                                                   (read in)))))))))
              (foo release))))
      (when builtin-info (return-from get-info builtin-info)))
    #+ASDF (try-use-file (merge-pathnames
                           (format nil "game-info/~D.sexp" release)
                           (asdf:system-source-directory :terraria-map-dump)))
    (error "Can't locate information for release ~D." release)))

(defun set-game-info (release)
  "Set the game-info data for RELEASE"
  (labels ((parse-color
             (color)
             (destructuring-bind (r g b a) color
               (%rgba r g b a)))
           (parse-color-list
             (color-list)
             (coerce (mapcar #'parse-color color-list)
                     '(vector color)))
           (parse-single-info
             (info-form)
             (destructuring-bind (name sets colors) info-form
               (values
                 (make-infos
                   :name name
                   :sets sets
                   :colors (parse-color-list colors))
                 (length colors))))
           (parse-info
             (info-list)
             (loop for id upfrom 0
                   for entry in info-list
                   for parsed = (multiple-value-list (parse-single-info entry))
                   collect (first parsed) into info
                   append (loop for i from 0 below (second parsed)
                                collect (cons id i)) into mapper
                   finally (return
                             (values
                               (coerce info '(vector infos))
                               (coerce mapper '(vector integer)))))))
    (let ((root-game-info
            (get-info release)))
      (setf *dirt-colors* (parse-color-list (cdr (assoc :dirt root-game-info))))
      (setf *empty-color* (parse-color (cdr (assoc :empty root-game-info))))
      (setf (values
              *block-info*
              *block-id-mapper*) (parse-info (cdr (assoc :tiles root-game-info))))
      (setf *fluid-colors* (parse-color-list (cdr (assoc :liquids root-game-info))))
      (setf *hell-color* (parse-color (cdr (assoc :hell root-game-info))))
      (setf *rock-colors* (parse-color-list (cdr (assoc :rock root-game-info))))
      (setf *sky-colors* (parse-color-list (cdr (assoc :sky root-game-info))))
      (setf (values
              *wall-info*
              *wall-id-mapper*) (parse-info (cdr (assoc :walls root-game-info))))))
  nil)

(defun paint-base-color (paint-id)
  (case paint-id
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

(defun tile-raw-rgba (tile &optional y profile)
  "COLOR of TILE, ignoring light"
  (let* ((base-color (tile-base-color tile y profile))
         (paint-color (paint-base-color (tile-paint-id tile)))
         (brightest-value (max (color-r base-color)
                               (color-g base-color)
                               (color-b base-color))))
    (if (zerop (tile-paint-id tile))
      base-color
      (flet ((scale-color
               (color factor)
               (%rgba (round (* factor (color-r color)))
                      (round (* factor (color-g color)))
                      (round (* factor (color-b color)))
                      (color-a base-color))))
        (case (tile-paint-id tile)
          (29 (scale-color paint-color
                           (* 3/10
                              (/ (if (= (color-b base-color) brightest-value)
                                   (max (color-r base-color) (color-g base-color))
                                   (color-b base-color)) 255))))
          (30 (let ((inverted-color (invert-color base-color)))
                (if (typep tile 'wall-tile)
                  (scale-color inverted-color 1/2)
                  inverted-color)))
          (otherwise (scale-color paint-color
                                  (/ brightest-value 255))))))))

(defun tile-rgba (tile &optional y profile)
  "COLOR of TILE, including light"
  (let ((unlit-color (tile-raw-rgba tile y profile))
        (light-amount (/ (tile-light-level tile) 255)))
    (if (= light-amount 1)
      unlit-color
      (%rgba (round (* (color-r unlit-color) light-amount))
             (round (* (color-g unlit-color) light-amount))
             (round (* (color-b unlit-color) light-amount))
             (color-a unlit-color)))))
