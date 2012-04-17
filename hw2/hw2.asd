(asdf:defsystem #:hw2
  :serial t
  :components ((:file "sdl")
	       (:file "util")
	       (:file "filter")
	       (:file "corner")
	       (:file "homography")
	       (:file "hw2"))
  :depends-on (#:png-read 
	       #:matlisp 
	       #:zpng
	       #:lispbuilder-sdl
	       #:cl-opengl
	       #:lispbuilder-sdl-image))