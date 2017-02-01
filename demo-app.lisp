(defun main ()
  (if (first (uiop:command-line-arguments))
    (let ((in-filename (make-pathname :defaults (first (uiop:command-line-arguments)))))
      (princ "Reading map...")(terpri)
      (let ((map (tmapdump:read-map in-filename)))
        (princ "World: ")(princ (tmapdump:minimap-world-name map))(terpri)
        (princ "Dumping map...")(terpri)
        (tmapdump:render-png map (make-pathname
                                   :type "png"
                                   :defaults in-filename))
        (princ "Done!")(terpri)))
    (progn
      (princ "Please supply a filename on the command line.")(terpri)))
  (quit))
