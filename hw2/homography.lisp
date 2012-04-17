
(in-package #:hw2)

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
		     (let ((h (m* V-t (transpose [0 0 0 0 0 0 0 0 1]))))
		       (values (transpose (reshape h 3 3))
			       A)))))
      (error "Need at least 4 correspondences")))
      

		
(defun test-identity-homography ()
  (let ((h (solve-homography-matrix 
	    (loop for i from 0 below 30 collect
	      (let ((v1 (random 500))
		    (v2 (random 500)))
	      (list (list v1 v2 1) (list (+ v1 100) v2 1)))))))
    (values h (normalize-matrix h))))
