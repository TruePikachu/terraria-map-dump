(defsystem :terraria-map-dump--demo-app
  :entry-point "cl-user::main"
  :depends-on (:terraria-map-dump)
  :components ((:file "demo-app")))
