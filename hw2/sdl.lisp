(in-package #:hw2)


;; Using SDL to interact with user to get correspodence points.
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

(defun draw-2d-texture (tex x y w h u1 v1 u2 v2 &optional (color '(1 1 1)))
  (gl:enable :texture-2d)
  (gl:bind-texture :texture-2d tex)
  (rectangle x y w h u1 v1 u2 v2 color))


(defun load-texture-from-img (img)
  (let* ((texture (car (gl:gen-textures 1)))
	 (iw (car (size (car img))))
	 (ih (cadr (size (car img))))
	 (texture-format :rgb) ;; rgb
	 (data (make-array (* iw ih 3) 
			   :element-type 'unsigned-byte
			   :initial-element 100)))
    ;; Prepare the texture data from image matrix
    (loop for y from 0 below ih do
	 (loop for x from 0 below iw do
	      (let ((pos (* (+ (* y iw) x) 3)))
		(setf (aref data pos) (conv-8-bit (matrix-ref (car img) x y)))
		(setf (aref data (+ pos 1)) (conv-8-bit (matrix-ref (cadr img) x y)))
		(setf (aref data (+ pos 2)) (conv-8-bit (matrix-ref (caddr img) x y))))))
    
    
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    
    
    (gl:tex-image-2d :texture-2d 0 :rgba
		     iw ih
		     0
		     texture-format
		     :unsigned-byte data)
    texture))




(defclass image ()
  ((tex :initarg :tex
	:accessor tex)
   (mat :initarg :mat
	:accessor mat)
   (width :initarg :width
	  :accessor width)
   (height :initarg :height
	   :accessor height)))


(defclass landmark ()
  ((pt1 :initarg :pt1
	:initform nil
	:accessor pt1)
   (pt2 :initarg :pt2
	:initform nil
	:accessor pt2)
   (color :initarg :color
	  :accessor color))
  (:documentation "Indicating the correspondence pair points"))


(defvar *cur-landmark* nil)
(defvar *landmarks* nil)

(defun click-handler (x y))
  

(defun make-image (img)
  (make-instance 'image 
		 :tex (load-texture-from-img img)
		 :mat img
		 :width (car (size (car img)))
		 :height (cadr (size (car img)))))

(defun draw (i1 i2)
  (setup-draw)

  (gl:load-identity)

  (gl:push-matrix)

  (with-slots (tex width height) i1
    (gl:translate (/ width 2) (/ height 2) 0)
    (draw-2d-texture tex 0 0 width height 0 0 1 1)
    (gl:translate (/ width 2) 0 0))
  
  (with-slots (tex width height) i2
    (gl:translate (/ width 2) 0 0)
    (draw-2d-texture tex 0 0 width height 0 0 1 1))
  

  (gl:pop-matrix)

  
  (gl:flush)
  (sdl:update-display))


(defun pick-image-point (img1 img2 num-of-points)
  (let* ((w1 (car (size (car img1))))
	 (h1 (cadr (size (car img1))))
	 (w2 (car (size (car img2))))
	 (h2 (cadr (size (car img2))))
	 (w (+ w1 w2))
	 (h (max h1 h2)))

    (sdl:with-init ()
      (sdl:window (+ w1 w2) (max h1 h2)
		  :title-caption "Image"
		  :icon-caption "Image"
		  :opengl t
		  :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    
      (setup-ortho-projection w h)

      ;; Load textures
      (let ((i1 (make-image img1))
	    (i2 (make-image img2)))
	       

	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event () (sdl:update-display))
	  (:mouse-button-down-event (:x x :y y) (format t "Click: (~A,~A)~%" x y))
	  (:idle () (draw i1 i2) (slime-conn)))
	
	;; Delete textures
	(gl:delete-textures (list (slot-value i1 'tex)
				  (slot-value i2 'tex)))))))

	 