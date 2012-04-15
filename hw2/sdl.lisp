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

(defun draw-line (p1 p2 color)
  (gl:disable :texture-2d)
  (apply 'gl:color color)


  (gl:line-width 1)
  (gl:begin :lines)
  (gl:vertex (car p1) (cadr p1))
  (gl:vertex (car p2) (cadr p2))
  (gl:end)
  (gl:enable :texture-2d))


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


(defun draw-point (point color size)
  (draw-line (list (- (car point) size) (- (cadr point) size))
	     (list (+ (car point) size) (+ (cadr point) size))
	     color)
  (draw-line (list (+ (car point) size) (- (cadr point) size))
	     (list (- (car point) size) (+ (cadr point) size))
	     color))


(defun draw-landmark (i1 i2 landmark size)
  (with-slots (pt1 pt2 color) landmark
    (when pt1
	(draw-point 
	 (list (car pt1) (- (height i1) (cadr pt1))) 
	 color size))
    (gl:translate (width i1) 0 0)
    (when pt2
      (draw-point 
       (list (car pt2) (- (height i1) (cadr pt2)))
       color size))))
    
      
(defvar *cur-landmark* nil)
(defvar *landmarks* nil)

(defun rand-color ()
  (list (/ (random 256) 255)
	(/ (random 256) 255)
	(/ (random 256) 255)))

(defun click-handler (i1 i2 x y)
  (format t "Click: (~A,~A)~%" x y)

  ;; Make a new landmark
  (when (null *cur-landmark*)
    (setf *cur-landmark* (make-instance 'landmark :color (rand-color))))
  
  (if (< x (width i1))
      (setf (pt1 *cur-landmark*) (list x y))
      (setf (pt2 *cur-landmark*) (list (- x (width i1)) y)))

  ;; When the landmark is filled, push to the list
  (when (and (pt1 *cur-landmark*) (pt2 *cur-landmark*))
      (push *cur-landmark* *landmarks*)
      (setf *cur-landmark* nil)))

(defun make-image (img)
  (make-instance 'image 
		 :tex (load-texture-from-img img)
		 :mat img
		 :width (car (size (car img)))
		 :height (cadr (size (car img)))))


(defun draw (i1 i2)
  (setup-draw)


  ;; Draw images
  
  (gl:push-matrix)
  (gl:load-identity)

  (with-slots ((tex1 tex) (w1 width) (h1 height)) i1
    (with-slots ((tex2 tex) (w2 width) (h2 height)) i2

      ;; Draw image 1
      (gl:translate (/ w1 2) (/ h1 2) 0)
      (draw-2d-texture tex1 0 0 w1 h1 0 0 1 1)


      ;; Draw image 2

      (gl:translate (/ w1 2) 0 0)

      (gl:translate (/ w2 2) 0 0)
      (draw-2d-texture tex2 0 0 w2 h2 0 0 1 1)

      ;; Draw transformed image
      (gl:push-matrix)
      (when *homography* 


	(gl:mult-matrix *homography*)

	(draw-2d-rect 0 0 w1 h1 '(1 0 0))
	#+nil
	(draw-2d-texture tex1 0 0 w1 h1 0 0 1 1))
      (gl:pop-matrix)))

  (gl:pop-matrix)



  ;; Draw landmarks
  (gl:push-matrix)
  (gl:load-identity)
  (if *cur-landmark* 
      (draw-landmark i1 i2 *cur-landmark* 10))
  (gl:pop-matrix)

  (dolist (l *landmarks*)
    (gl:push-matrix)
    (gl:load-identity)
    (draw-landmark i1 i2 l 5)
    (gl:pop-matrix))



  (gl:flush)
  (sdl:update-display))


(defun pick-image-point (img1 img2)
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
	  (:mouse-button-down-event (:x x :y y) (click-handler i1 i2 x y))
	  (:idle () (draw i1 i2) (slime-conn)))
	
	;; Delete textures
	(gl:delete-textures (list (slot-value i1 'tex)
				  (slot-value i2 'tex)))))))


(defvar *points* nil)	 
(defun get-points-from-landmarks ()
  (setf *points* 
	(loop for l in *landmarks* collect
	     (list (append (pt1 l) '(1)) 
		   (append (pt2 l) '(1))))))

(defvar *gl-points* nil)
(defun get-gl-points ()
  (setf *gl-points* (loop for (p1 p2) in *points*
	  collect (list (list (car p1) (- 576 (cadr p1)) (caddr p1))
			(list (car p2) (- 576 (cadr p2)) (caddr p2))))))

(defun 2D->3D-transfom (2d-mat)
  "Convert the 2D transform matrix to 3D transform matrix"
  (assert (and (= (car (size 2d-mat)) 3)
	       (= (cadr (size 2d-mat)) 3)))
  (let ((3d-mat (make-float-matrix 4 4)))
    (setf (matrix-ref 3d-mat 0 0) (matrix-ref 2d-mat 0 0))
    (setf (matrix-ref 3d-mat 1 0) (matrix-ref 2d-mat 1 0))
    (setf (matrix-ref 3d-mat 0 1) (matrix-ref 2d-mat 0 1))
    (setf (matrix-ref 3d-mat 1 1) (matrix-ref 2d-mat 1 1))
    (setf (matrix-ref 3d-mat 3 0) (matrix-ref 2d-mat 2 0))
    (setf (matrix-ref 3d-mat 3 1) (matrix-ref 2d-mat 2 1))
    (setf (matrix-ref 3d-mat 0 3) (matrix-ref 2d-mat 0 2))
    (setf (matrix-ref 3d-mat 1 3) (matrix-ref 2d-mat 1 2))
    (setf (matrix-ref 3d-mat 3 3) (matrix-ref 2d-mat 2 2))
    
    (setf (matrix-ref 3d-mat 2 2) 1)
    3d-mat))

(defun 3D-transform->arr (mat)
  (convert-to-lisp-array (reshape mat 16 1)))
    
(defvar *homography* nil)

(defun compute-homography (points)
  (setf *homography* (3D-transform->arr
		      (2D->3D-transfom 
		       (solve-homography-matrix points)))))




