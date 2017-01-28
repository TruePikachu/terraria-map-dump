(defsystem :terraria-map-dump
  :description "Terraria Map Dumper"
  :version "1.0"
  :author "Chris Dusto <cricket_lover@live.com>"
  :bug-tracker "https://github.com/TruePikachu/terraria-map-dump/issues"
  :licence "MIT License"
  :depends-on (:deflate :flexi-streams)
  :components ((:file "binary-reader")
               (:file "game-info"
                      :depends-on ("map-header"))
               (:file "map-header"
                      :depends-on ("binary-reader"))
               (:file "map-tile")
               (:file "tile-reader"
                      :depends-on ("binary-reader"
                                   "map-header"
                                   "map-tile"))))
