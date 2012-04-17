(in-package #:hw2)

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

;; Generating the sample of gaussian filter matrix
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

;; Generating DoG image
(defun gen-dog-images (img)
  (let* ((smoothed (filter-2d img *filter-gaussian*))
	 (sobel-x (filter-2d smoothed *filter-sobel-x*))
	 (sobel-y (filter-2d smoothed *filter-sobel-y*)))
    (list sobel-x sobel-y)))
