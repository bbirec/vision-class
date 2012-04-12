(defpackage #:hw2
  (:use :cl :png-read :matlisp)
  (:shadow :real))

(in-package #:hw2)

(defparameter *path* "/Users/bbirec/Dropbox/Classes/vision/hw2/")

;; Image load
(defparameter *img1* (read-png-file
		      (concatenate 'string *path* "view1.png")))
(defparameter *img2* (read-png-file
		      (concatenate 'string *path* "view2.png")))

(defun img-matrix (img)
  (let ((data (image-data img))
	(w (width img))
	(h (height img)))
    

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
		   (multiple-value-bind (V-t D V) (svd B :A) 
		     (declare (ignore D) (ignore V))
		     V-t
		     (m* V-t (transpose [0 0 0 0 0 0 0 0 1])))))
      (error "Need 4 correspondences")))
      


;;  linear filters
(defparameter *filter-gaussian*  [1/16 4/16 6/16 4/16 1/16])

(defparameter *filter-gradient-x* 
  [[-1/8 0 1/8]'
  [-2/8 0 2/8]'
  [-1/8 0 1/8]'])

(defparameter *filter-gradient-y*
  [[-1/8 -2/8 -1/8]'
  [0 0 0]'
  [1/8 2/8 1/8]'])
	      
;; Convolution

(defun convolution (A B))




  