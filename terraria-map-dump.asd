(defsystem :terraria-map-dump
  :description "Terraria Map Dumper"
  :version "1.0"
  :author "Chris Dusto <cricket_lover@live.com>"
  :bug-tracker "https://github.com/TruePikachu/terraria-map-dump/issues"
  :licence "MIT License"
  :depends-on (:cl-reexport :deflate :flexi-streams :zpng)
  :components ((:file "binary-reader")
               (:file "color")
               (:file "map"
                      :depends-on ("binary-reader"
                                   "tile"))
               (:file "package"
                      :depends-on ("color"
                                   "map"
                                   "render-png"
                                   "tile"))
               (:file "render-png"
                      :depends-on ("color"
                                   "map"
                                   "tile"))
               (:file "tile"
                      :depends-on ("binary-reader"
                                   "color"))))
