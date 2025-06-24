(asdf:defsystem #:sorcery-sdl2
  :version "1.0.0"
  :author "Frédéric FERRERE"
  :license "MIT"
  :serial t
  :depends-on (#:alexandria
	       #:bordeaux-threads
	       #:bit-smasher
	       #:sdl2
	       #:sb-sprof
	       #:sdl2-engine
	       #:array-operations
               #:quadtree
	       #:cl-entity-system)
  :components ((:file "package")
	       (:file "conf")
	       (:file "translate")
	       (:file "data")
	       (:file "components")
               (:file "world")
	       (:file "keyboard-system")
	       (:file "move-system")
	       (:file "animate-system")
	       (:file "collide-system")
	       (:file "render-system")
	       (:file "bag-system")
	       (:file "clear-round-system")
	       (:file "time-system")
               (:file "game")
	       (:file "main")
	       ))
