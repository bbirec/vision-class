

(in-package #:hw2)


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


(defun has-corner? (dog-img x y)
  (let ((R (harris-corner-detector (rgb->grayscale (first dog-img))
				   (rgb->grayscale (second dog-img))
				   x y 9 2)))
    (values (> R 1) R)))
  
(defun search-corner (dog-img x y size)
  "Finding the position of corner by checking the near pixel with the harris corner detector"

  (let ((img-x (rgb->grayscale (first dog-img)))
	(img-y (rgb->grayscale (second dog-img))))
  
    (cdr (reduce 
	  #'(lambda (x y) (if (> (car x) (car y)) x y))
	  (loop for dy from (- size) to size append
	       (loop for dx from (- size) to size collect
		    (let ((px (+ dx x))
			  (py (+ dy y)))
		      ;; Finding the largest R value
		      (list (harris-corner-detector img-x img-y px py 9 2) px py))))))))