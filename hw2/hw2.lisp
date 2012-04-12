(defpackage #:hw2
  (:use :cl :png-read :matlisp)
  (:shadow :real))

(in-package #:hw2)

(defparameter *path* "/Users/bbirec/Dropbox/Classes/vision/hw2/")

;; Image load & write

(defun load-image (filename)
  "Converting the input image to matrix form"
  (let* ((img (read-png-file (concatenate 'string *path* filename)))
	 (data (image-data img))
	 (w (width img))
	 (h (height img))
	 (mat-R (make-float-matrix w h))
	 (mat-G (make-float-matrix w h))
	 (mat-B (make-float-matrix w h)))
    (loop for y from 0 below h do
	 (loop for x from 0 below w do
	      (setf (matrix-ref mat-R x y) (aref data x y 0))
	      (setf (matrix-ref mat-G x y) (aref data x y 1))
	      (setf (matrix-ref mat-B x y) (aref data x y 2))))
    (list mat-R mat-G mat-B)))




(defun clamp (x min max)
  (if (< x min) min
      (if (> x max) max x)))

(defun conv-8-bit (v)
  (round (clamp v 0.0 255.0)))

(defun write-image (filename mat-R mat-G mat-B)
  (let* ((s (size mat-R))
	 (w (car s))
	 (h (cadr s))
	 (png (make-instance 'zpng:png
			    :color-type :truecolor
			    :width w
			    :height h))
	 (image (zpng:data-array png)))
    (loop for y from 0 below h do
	 (loop for x from 0 below w do
	      (setf (aref image y x 0) (conv-8-bit (matrix-ref mat-R x y)))
	      (setf (aref image y x 1) (conv-8-bit (matrix-ref mat-G x y)))
      	      (setf (aref image y x 2) (conv-8-bit (matrix-ref mat-B x y)))))
    (zpng:write-png png (concatenate 'string *path* filename))))
			    


;; Matlisp

(defmacro join* (position &rest mats)
  (reduce #'(lambda (a b) `(join ,a ,b ,position)) mats))



;; Homography matrix

(defun solve-homography-matrix (point-pairs)
  (if (= (length point-pairs) 4)
      (labels ((make-A-matrix ()
		 (let ((mat
			;; 9x2 matrix for each point pair
			(loop for (p1 p2) in point-pairs collect
			     (destructuring-bind (x y w) p2
			       (let* ((mp1 (transpose (make-float-matrix p1)))
				      (m00 (make-float-matrix 1 3))
				      (m01 (m* (- w) mp1))
				      (m02 (m* y mp1))
				      (m10 (m* w mp1))
				      (m11 (make-float-matrix 1 3))
				      (m12 (m* (- x) mp1)))
				 (join* :vertical
					(join* :horizontal m00 m01 m02)
					(join* :horizontal m10 m11 m12)))))))

		   ;; Join the four matrix in vertical
		   (reduce #'(lambda (a b) (join a b :vertical)) mat))))
		 
		 (let* ((A (make-A-matrix))
			(B (m* (transpose A) A)))
		   (multiple-value-bind (V-t _D _V) (svd B :A) 
		     (declare (ignore _D) (ignore _V))
		     (m* V-t (transpose [0 0 0 0 0 0 0 0 1])))))
      (error "Need 4 correspondences")))
      


;; Linear filters
(defparameter *filter-gaussian* 
  [[1/256 4/256 6/256 4/256 1/256]'
  [4/256 16/256 24/256 16/256 4/256]'
  [6/256 24/256 36/256 24/256 6/256]'
  [4/256 16/256 24/256 16/256 4/256]'
  [1/256 4/256 6/256 4/256 1/256]'])

(defparameter *filter-sobel-x* 
  [[-1/8 0 1/8]'
  [-2/8 0 2/8]'
  [-1/8 0 1/8]'])

(defparameter *filter-sobel-y*
  [[-1/8 -2/8 -1/8]'
  [0 0 0]'
  [1/8 2/8 1/8]'])
	      
;; Convolution

(defun conv-2d (img filter)
  "2D image convolution operation"
  (let* ((is (size img))
	 (fs (size filter))
	 (diff (floor (/ (car fs) 2)))
	 (offset (- diff))
	 (offset2 (* offset 2))
	 (output (make-float-matrix (+ (car is) offset2)
				    (+ (cadr is) offset2))))
	 
    ;; Filter should be odd size
    (assert (and (oddp (car fs)) (oddp (cadr fs))))

    (loop for iy from diff below (+ (cadr is) offset) do
       (loop for ix from diff below (+ (car is) offset) do
	    
	    ;; Loop filter values
	    (let ((value 0))
	    
	    (loop for fy from 0 below (cadr fs) do
		 (loop for fx from 0 below (car fs) do
		      (setf value (+ value 
				     (* (matrix-ref filter fy fx)
					(matrix-ref img 
						    (+ ix fy offset) 
						    (+ iy fx offset)))))))
	    ;; Output matrix
	    (setf (matrix-ref output (+ ix offset) (+ iy offset))
		  value))))
    output))

  
;; Applying 2d linear image
(defun filter-2d (img &rest filter)
  (destructuring-bind (r g b) img
    (loop for f in filter do
	 (setf r (conv-2d r f))
	 (setf g (conv-2d g f))
	 (setf b (conv-2d b f)))
    (list r g b)))



(defun filter-2d-save (filename img &rest filter)
  (destructuring-bind (r g b) (apply #'filter-2d (cons img filter))
    (write-image filename r g b)))






;; Generating DoG image
(defun gen-dog-images (img)
  (let* ((smoothed (filter-2d img *filter-gaussian*))
	 (sobel-x (filter-2d smoothed *filter-sobel-x*))
	 (sobel-y (filter-2d smoothed *filter-sobel-y*)))
    (list sobel-x sobel-y)))




(defparameter *img1* (load-image "view1.png"))
(defparameter *img2* (load-image "view2.png"))


(defun gen-images ()
  (destructuring-bind (dx dy) (gen-dog-images *img1*)
    (write-image "img1_dx.png" (first dx) (second dx) (third dx))
    (write-image "img1_dy.png" (first dy) (second dy) (third dy))))