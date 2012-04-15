(in-package #:hw2)


;; Using SDL to interacte with user
;; Getting points to compute the homography matrix.
;; Representing the stitched image in 3D space with OpenGL.

(defun setup-ortho-projection (width height)
  "Setup OpenGL to use for 2D graphics"
  (gl:viewport 0 0 width height)

  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width 0 height -1 1)

  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun setup-draw ()
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear :color-buffer-bit)
  (gl:color 1 1 1))

(defun slime-conn ()
  (let ((connection
       (or swank::*emacs-connection* (swank::default-connection))))
  (when (and connection (not (eql swank:*communication-style* :spawn)))
    (swank::handle-requests connection t))))

(defun rectangle (x y width height 
		  &optional (u1 0) (v1 0) (u2 1) (v2 1) (c '(1 1 1)))
  (gl:with-primitive :quads
    (let* ((w/2 (/ width 2.0))
	   (h/2 (/ height 2.0))
	   (x1 (- x w/2))
	   (x2 (+ x w/2))
	   (y1 (- y h/2))
	   (y2 (+ y h/2)))
      
      (gl:tex-coord u1 v2)
      (apply 'gl:color c)
      (gl:vertex x1 y1 0)

      (gl:tex-coord u2 v2)
      (apply 'gl:color c)
      (gl:vertex x2 y1 0)

      (gl:tex-coord u2 v1)
      (apply 'gl:color c)
      (gl:vertex x2 y2 0)

      (gl:tex-coord u1 v1)
      (apply 'gl:color c)
      (gl:vertex x1 y2 0))))

(defun draw-2d-rect (x y w h color)
  (gl:disable :texture-2d)
  (rectangle x y w h 0 0 1 1 color))


(defun draw ()
  (setup-draw)

  (gl:load-identity)

  (gl:mult-matrix #(1 0 0 0 0 1 0 0 0 0 1 0 100 100 0 1))
  
  #+nil
  (gl:translate 100 100 0)

  (draw-2d-rect 0 0 100 100 '(1 0 0))
  
  (gl:flush)
  (sdl:update-display))



(defun pick-image-point (img num-of-points)
  (let ((width (car (size (car img))))
	(height (cadr (size (car img)))))

    (sdl:with-init ()
      (sdl:window width height
		  :title-caption "Image"
		  :icon-caption "Image"
		  :opengl t
		  :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    
      (setup-ortho-projection width height)
    

      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:mouse-button-down-event (:x x :y y) (format t "Click: (~A,~A)~%" x y))
	(:idle () (draw) (slime-conn))))))

	 