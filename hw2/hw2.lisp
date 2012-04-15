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
  "Return 3x3 homography matrix from the correspondence points"
  (if (>= (length point-pairs) 4)
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
		     (transpose (reshape (m* V-t (transpose [0 0 0 0 0 0 0 0 1])) 3 3)))))
      (error "Need at least 4 correspondences")))
      

(defun normalize-matrix (mat)
  (assert (square-matrix-p mat))
  (let ((s (matrix-ref mat 
		       (- (car (size mat)) 1)
		       (- (car (size mat)) 1))))
    (m/ mat s)))
		       

;; Linear filters
(defparameter *filter-gaussian* 
  [
  [1/256 4/256 6/256 4/256 1/256];
  [4/256 16/256 24/256 16/256 4/256];
  [6/256 24/256 36/256 24/256 6/256];
  [4/256 16/256 24/256 16/256 4/256];
  [1/256 4/256 6/256 4/256 1/256];
  ])

(defparameter *filter-sobel-x* 
  [
  [-1/8 0 1/8];
  [-2/8 0 2/8];
  [-1/8 0 1/8];
  ])

(defparameter *filter-sobel-y*
  [
  [-1/8 -2/8 -1/8];
  [0 0 0];
  [1/8 2/8 1/8];
  ])
	      
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
    (write-image "img1_dy.png" (first dy) (second dy) (third dy)))
  (destructuring-bind (dx dy) (gen-dog-images *img2*)
    (write-image "img2_dx.png" (first dx) (second dx) (third dx))
    (write-image "img2_dy.png" (first dy) (second dy) (third dy))))


;; Finding corner based on the point picked manually.


(defparameter *patch-size* 9)
#+nil
(defparameter *dog-img1* (gen-dog-images *img1*))
#+nil
(defparameter *dog-img2* (gen-dog-images *img2*))

(defun msub (mat x y w h)
  "Getting a rectangluar region of given matrix."
  (assert (and (>= x 0) (>= y 0)
	       (>= w 1) (>= h 1)
	       (<= (+ x w) (cadr (size mat)))
	       (<= (+ y h) (car (size mat)))))

  (let ((m (make-float-matrix h w)))
    (loop for my from 0 below h do
	 (loop for mx from 0 below w do
	      (setf (matrix-ref m mx my)
		    (matrix-ref mat (+ mx x) (+ my y)))))
    m))

(defun msub-center (mat x y size)
  "Getting the subset of matrix where the position is centered."
  (assert (and (>= size 1) (oddp size)))
  (let* ((diff (floor (/ size 2)))
	 (mx (- x diff))
	 (my (- y diff)))
    (msub mat mx my size size)))

(defun gaussian-filter (size sigma)
  "Sample the gaussian in matrix form."
  (assert (oddp size))
  (let ((matrix (make-float-matrix size size))
	(diff (floor (/ size 2))))
    (loop for y from 0 below size do
	 (loop for x from 0 below size do
	      (let ((fx (- x diff))
		    (fy (- y diff)))
		(setf (matrix-ref matrix x y)
		      (/ (exp (- (/ (+ (* fx fx) (* fy fy))
				    (* 2 sigma sigma))))
			 (* 2 pi sigma sigma))))))
    matrix))


(defun harris-corner-detector (img-x img-y x y patch-size sigma)
  "Check whether the patch has corner or not."
  (let ((ix (msub-center img-x x y patch-size))
	(iy (msub-center img-y x y patch-size)))
    (let ((ix2 (m.* ix ix))
	  (iy2 (m.* iy iy))
	  (ixiy (m.* ix iy))
	  (gf (gaussian-filter patch-size sigma)))
      ;; Applying the larger gaussian for each matrix
      (let ((v-ix2 (sum (m.* ix2 gf)))
	    (v-iy2 (sum (m.* iy2 gf)))
	    (v-ixiy (sum (m.* ixiy gf))))
	(let* ((result (svd 
			[[v-ix2 v-ixiy];
			[v-ixiy v-iy2]]))
	       (ld-x (matrix-ref result 0))
	       (ld-y (matrix-ref result 1)))
	  ;; Harris Corner Detector
	  (- (* ld-x ld-y) (* 0.06 (+ ld-x ld-y) (+ ld-x ld-y))))))))

(defun rgb->grayscale (img)
  (m./ (m.+ (m.+ (car img) (cadr img)) (caddr img)) 3))

(defun has-corner? (dog-img x y)
  (let ((R (harris-corner-detector (rgb->grayscale (first dog-img))
				   (rgb->grayscale (second dog-img))
				   x y 9 2)))
    (values (> R 1) R)))
  

