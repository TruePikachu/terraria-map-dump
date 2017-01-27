(defsystem :terraria-map-dump
  :description "Terraria Map Dumper"
  :version "1.0"
  :author "Chris Dusto <cricket_lover@live.com>"
  :bug-tracker "https://github.com/TruePikachu/terraria-map-dump/issues"
  :licence "MIT License"
  :depends-on ()
  :components ((:file "binary-reader")
               (:file "map-header"
                      :depends-on ("binary-reader"))))
