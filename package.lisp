(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :terraria-map-dump)
    (delete-package :terraria-map-dump))
  (make-package :terraria-map-dump :nicknames '(:tmapdump) :use '(:common-lisp :cl-reexport)))
(in-package :terraria-map-dump)

(dolist (package (list :tmapdump.color
                       :tmapdump.map
                       :tmapdump.render-png
                       :tmapdump.tile))
  (reexport-from package))
