(asdf:defsystem #:hw2
  :serial t
  :components ((:file "hw2"))
  :depends-on (#:png-read 
	       #:matlisp 
	       #:zpng
	       #:lispbuilder-sdl
	       #:cl-opengl
	       #:lispbuilder-sdl-image))