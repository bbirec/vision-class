(in-package #:hw2)

(defparameter *path* "/Users/bbirec/Dropbox/Classes/vision/hw2/")

;; Image load & write

(defun load-image (filename)
  (load-image-from-file (concatenate 'string *path* filename)))

(defun write-image (filename mat-R mat-G mat-B)
  (write-image-to-file (concatenate 'string *path* filename) mat-R mat-G mat-B))


(defparameter *img1* nil)
(defparameter *img2* nil)
(defparameter *dog-img1* nil)
(defparameter *dog-img2* nil)
  
(defun init-hw2 ()
  ;; Load images
  (setf *img1* (load-image "view1.png")
	*img2* (load-image "view2.png"))

  (setf *dog-img1* (gen-dog-images *img1*)
	*dog-img2* (gen-dog-images *img2*)))
  
	
(defun save-dog-images ()
  (destructuring-bind (dx dy) *dog-img1*
    (write-image "img1_dx.png" (first dx) (second dx) (third dx))
    (write-image "img1_dy.png" (first dy) (second dy) (third dy)))
  (destructuring-bind (dx dy) *dog-img2*
    (write-image "img2_dx.png" (first dx) (second dx) (third dx))
    (write-image "img2_dy.png" (first dy) (second dy) (third dy))))
  

;; Classes


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

(defvar *points* nil)	 
(defvar *homography* nil)

(defvar *square-points* nil)
(defvar *square-homography* nil)

(defvar *pick-mode* t)






(defun make-image (img)
  (make-instance 'image 
		 :tex (load-texture-from-img img)
		 :mat img
		 :width (car (size (car img)))
		 :height (cadr (size (car img)))))




(defun get-points-from-landmarks ()
  (setf *points* 
	(loop for l in *landmarks* collect
	     (list (append (pt1 l) '(1)) 
		   (append (pt2 l) '(1))))))
    

(defun compute-homography (points)
  (let ((H (2D->3D-transfom 
	    (normalize-matrix (solve-homography-matrix points)))))
    (values (3D-transform->arr H) H)))


(defun test-homography (points)
  (multiple-value-bind (arr H) (compute-homography points)
    (format t "H: ~A~%" H)
    (format t "H * 0: ~A~%" (normalize-vector (m* H [0 0 0 1]')))
    (format t "H * 0: ~A~%" (normalize-vector (m* H [720 0 0 1]')))
    (format t "H * 0: ~A~%" (normalize-vector (m* H [0 576 0 1]')))
    (format t "H * 0: ~A~%" (normalize-vector (m* H [720 576 0 1]')))))


  
(defun clear-homography ()
  (setf *homography* nil)
  (setf *cur-landmark* nil)
  (setf *landmarks* nil))


;; SDL specific functions

(defun draw-landmark (i1 i2 landmark size)
  (with-slots (pt1 pt2 color) landmark
    (when pt1
	(draw-point 
	 (list (car pt1) (cadr pt1))
	 color size))
    (gl:translate (width i1) 0 0)
    (when pt2
      (draw-point 
       (list (car pt2) (cadr pt2))
       color size))))



(defun draw-result (i1 i2)
  (with-slots ((tex1 tex) (w1 width) (h1 height)) i1
    (with-slots ((tex2 tex) (w2 width) (h2 height)) i2

      (let ((s 1))
	(gl:scale s s 1))
      (gl:translate 1000 0 0)

      (if *square-homography*
	  (gl:mult-matrix *square-homography*))

      
      (gl:with-pushed-matrix
	(gl:translate (/ w2 2) (/ h2 2) 0)
	(draw-2d-texture tex2 0 0 w2 h2 0 0 1 1))


      (when *homography* 
	(gl:with-pushed-matrix
	  (gl:mult-matrix *homography*)

	  (gl:translate (/ w1 2) (/ h1 2) 0)
	  (draw-2d-texture tex1 0 0 w1 h1 0 0 1 1 '(1 1 1 0.5)))))))




(defun draw-pick (i1 i2)
  
  (with-slots ((tex1 tex) (w1 width) (h1 height)) i1
    (with-slots ((tex2 tex) (w2 width) (h2 height)) i2

      (gl:with-pushed-matrix
      ;; Draw image 1
      (gl:translate (/ w1 2) (/ h1 2) 0)
      (draw-2d-texture tex1 0 0 w1 h1 0 0 1 1)

      ;; Draw image 2
      (gl:translate (/ w1 2) 0 0)
      (gl:translate (/ w2 2) 0 0)
      (draw-2d-texture tex2 0 0 w2 h2 0 0 1 1))))

  ;; Draw current landmark
  (gl:with-pushed-matrix
    (if *cur-landmark* 
	(draw-landmark i1 i2 *cur-landmark* 10)))

  ;; Draw landmarks
  (dolist (l *landmarks*)
    (gl:with-pushed-matrix
    (draw-landmark i1 i2 l 5))))


      



(defun draw (i1 i2)
  (setup-draw)

  ;; Draw images
  (if *pick-mode*
      (draw-pick i1 i2)
      (draw-result i1 i2))

  (gl:flush)
  (sdl:update-display))

(defun click-handler (i1 i2 x y)
  (format t "Click: (~A,~A)~%" x y)

  (if *pick-mode*
      (progn

	;; Make a new landmark
	(when (null *cur-landmark*)
	  (setf *cur-landmark* (make-instance 'landmark :color (rand-color))))
  
	(if (< x (width i1))
	    (setf (pt1 *cur-landmark*) (list x (- (height i1) y)))
	    (setf (pt2 *cur-landmark*) (list (- x (width i1)) (- (height i1) y))))

	;; When the landmark is filled, push to the list
	(when (and (pt1 *cur-landmark*) (pt2 *cur-landmark*))
	  (push *cur-landmark* *landmarks*)
	  (setf *cur-landmark* nil))

	;; If the points are collected, compute the homography matrix
	(when (>= (length *landmarks*) 4)
	  (setf *homography* (compute-homography (get-points-from-landmarks)))))

      (progn
	;; TODO : getting real point with tranform matrix.
	(push (list (- x 1000) (- (height i1) y) 1) *square-points*)

	(when (= (length *square-points*) 4)
	  ;; Compute homography matrix
	  (setf *square-homography*
		(compute-homography
		 (mapcar #'list 
			 *square-points*
			 '((0 0 1) (300 0 1) (300 300 1) (0 300 1)))))
	  
	  ;; Clear the homography matrix
	  (setf *square-points* nil))

	)))



(defun key-handler (key)
  (cond ((sdl:key= key :sdl-key-space) (setf *pick-mode* (not *pick-mode*)))
	((sdl:key= key :sdl-key-escape) (clear-homography))))



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
	  (:key-down-event (:key key) (key-handler key))
	  (:mouse-button-down-event (:x x :y y) (click-handler i1 i2 x y))
	  (:idle () (draw i1 i2) (slime-conn)))
	
	;; Delete textures
	(gl:delete-textures (list (slot-value i1 'tex)
				  (slot-value i2 'tex)))))))
