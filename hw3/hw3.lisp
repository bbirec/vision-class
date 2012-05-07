
(defpackage #:hw3
  (:use :cl :matlisp :png-read)
  (:shadow :real))

(in-package #:hw3)

;; RANSAC

(defun find-hypothesis (points)
  "Return two sample points from the given point list"
  (assert (>= (length points) 2))
  (let* ((p1 (nth (random (length points)) points))
	 (remain (remove p1 points)))
    (loop for p2 in remain
	 unless (= (car p1) (car p2))
	 return (list p1 p2))))


(defun remove-points (points p1 p2)
  (remove p2 (remove p1 points)))


(defun RANSAC (iterations threshold points outlier-ratio)
  (labels ((inlier-p (a b x y)
	     "Check the point is inlier or not and return distance"
	     (let ((dist (/ (abs (+ (* a x) (* -1 y) b))
			     (sqrt (+ (* a a) 1)))))
	       (values (> threshold dist) dist)))

	   (gen-inliers (a b num-of-points)
	     "Generate inliers with the given line"
	     (loop for x from 0 below num-of-points collect
		  (let ((ep (/ (random threshold) threshold)))
		    (list x (+ (* a x) b ep)))))

	   (gen-outliers (a b num-of-points)
	     "Generate outliers with the given line"
	     (loop for x from 0 below num-of-points collect
		  (list x 
			(loop for y = (random 10000)
			   when (not (inlier-p a b x y))
			   return y))))

	   (get-inliers (points)
	     "Find inliers by picking random hypothesis points."
	     (destructuring-bind (p1 p2) (find-hypothesis points)
	       (let ((remains (remove-points points p1 p2)))
		 (destructuring-bind (x1 y1) p1
		   (destructuring-bind (x2 y2) p2
		   (assert (not (= x1 x2)))
		   (let* ((a (/ (- y2 y1) (- x2 x1)))
			  (b (- y1 (* a x1))))
		     (loop for (x y) in remains
			  if (inlier-p a b x y)
			  collect (list x y))))))))

	   (estimate-line (points)
	     (let ((M (make-float-matrix (length points) 2))
		   (k (make-float-matrix (length points) 1)))
	       (loop for (x y) in points 
		  for r from 0 do
		    (setf (matrix-ref M r 0) x)
		    (setf (matrix-ref M r 1) 1)
		    (setf (matrix-ref k r 0) y))
	       (let ((M-t (transpose M)))
		 (m* (m* (m/ (m* M-t M)) M-t) k)))))
		     
		   
    (let* ((a (random 100))
	   (b (random 100))
	   (inliers (gen-inliers a b (floor (* points (- 1 outlier-ratio)))))
	   (outliers (gen-outliers a b (floor (* points outlier-ratio))))
	   (points (append inliers outliers))
	   (max-points nil))
      (format t "~A inliers and ~A outliers"
	      (length inliers)
	      (length outliers))
      (format t "Line (a b) = (~A ~A)~%" a b)

      ;; Find best hypothesis
      (dotimes (i iterations)
	(let ((inliers (get-inliers points)))
	  (if (> (length inliers) (length max-points))
	      (setf max-points inliers))))

      (format t "Chosen the best hypothesis points : ~A points~%"
	      (length max-points))


      ;; Estimate the line
      (let* ((m (estimate-line max-points))
	     (e-a (matrix-ref m 0 0))
	     (e-b (matrix-ref m 1 0)))
	(format t "Estimated line (a b) = (~A ~A)~%" e-a e-b)
	(format t "Error : (~A ~A)~%" 
		(abs (- e-a a)) 
		(abs (- e-b b)))))))
	


;; Image clustering

(defun pick-k-random (k start end)
  "Pick k random values between start and end"
  (assert (>= (- end start) k))
  (let ((values nil))
    (loop for v = (- (random end) start) 
       until (>= (length values) k)
       do (pushnew v values))
    values))

(defun dist-2 (v1 v2)
  (assert (= (length v1) (length v2)))
  (reduce #'+ (mapcar #'(lambda (x) (* x x)) (mapcar #'- v1 v2))))

(defun sum-list (v1 v2)
  (mapcar #'+ v1 v2))

(defun find-cluster-idx (k-means d)
  (let ((min-dist (dist-2 (car k-means) d))
	(idx 0))
    (loop for k in (cdr k-means)
       for i from 1 do
	 (let ((dist (dist-2 k d)))
	   (when (< dist min-dist)
	     (setf min-dist dist)
	     (setf idx i))))
    (values idx min-dist)))
  
(defun p= (p1 p2)
  (and (= (length p1) (length p2))
       (every #'zerop (mapcar #'- p1 p2))))
  
(defun cluster= (c1 c2)
  (and (= (length c1) (length c2))
       (loop for p in c1 always
	    (member p c2 :test #'p=))))

(defun cluster-center (cluster)
  (let ((len (length cluster))
	(s (reduce #'sum-list cluster)))
    (mapcar #'(lambda (x) (/ x len)) s)))
       

(defun k-means (k data &optional (max-iteration 200))
  "Perform k-means clustering algorithm"
  (format t "Performing ~A-means algorithm with ~A points~%" k (length data))
  (let ((k-means nil)
	(clusters (make-array k :initial-element nil)))

    ;; Select k-means randomly
    (let ((init-idx (pick-k-random k 0 (length data))))
      (loop for idx in init-idx do
	   (push (nth idx data) k-means)))

    ;; Iterate until convergence of k-means
    (loop for i below max-iteration do
	 (format t "Iteration ~A~%" i)

	 ;; Assignment step
	 (let ((new-clusters (make-array k :initial-element nil)))
	   ;; Warning : Performance bottleneck
	   (loop for d in data do
		(let ((idx (find-cluster-idx k-means d)))
		  (push d (aref new-clusters idx))))

	   ;; Terminate if the clusters are same with the last one
	   (if (loop for j below k always
		    (cluster= (aref clusters j) (aref new-clusters j)))
	       (progn
		 (format t "End of cluster~%")
		 (return-from nil))
	       (progn 
		 ;; Set new clusters
		 (setf clusters new-clusters)

		 ;; Update step
		 (setf k-means 
		       (loop for j below k collect
			    (cluster-center (aref clusters j))))))))
			    
    (values clusters k-means)))

(defun saving-clusters (directory w h clusters)
  (loop for cluster in clusters 
     for i from 0 do
       (let* ((png (make-instance 'zpng:png
			    :color-type :truecolor
			    :width w
			    :height h))
	      (image (zpng:data-array png)))

	 (loop for c in cluster do
	      (let ((x (nth 0 c))
		    (y (nth 1 c))
		    (r (nth 2 c))
		    (g (nth 3 c))
		    (b (nth 4 c)))
		(setf (aref image y x 0) r
		      (aref image y x 1) g
		      (aref image y x 2) b)))

	 (let ((filepath (format nil "~A/~A_~A.png" directory "output" i)))
	   (zpng:write-png png filepath)))))


(defun find-super-pixel (filepath pixel-count)
  (let* ((img (read-png-file filepath))
	 (data (image-data img))
	 (w (width img))
	 (h (height img)))
	 
    (let ((d (loop for y below h append
		  (loop for x below w collect
		       (list x y 
			     (aref data x y 0)
			     (aref data x y 1)
			     (aref data x y 2))))))
      (let* ((clusters (coerce (time (k-means pixel-count d)) 'list))
	     (clusters-pixel-counts (mapcar #'length clusters)))
	(format t "Clustering: ~A~%" clusters-pixel-counts)
	(saving-clusters "/Users/bbirec/Dropbox/Classes/vision/hw3" 
			 w h clusters)))))

	 
  
	      
;; Nomalized cut algorithm

(defun weight-function (super-pixel1 super-pixel2)
  ;; Euclidian distance of cluster center
  (let ((center1 (cluster-center super-pixel1))
	(center2 (cluster-center super-pixel2)))
    (dist-2 center1 center2)))


(defun make-affinity-matrix (super-pixels)
  "W[i,j] = w(i,j)"
  ;; TODO : do not repeat the calculation
  (let* ((n (length super-pixels))
	 (mat (make-float-matrix n n)))
    (loop for j below n do
       (loop for i below n do
	    (setf (matrix-ref mat i j)
		  (weight-function (nth i super-pixels)
				   (nth j super-pixels)))))
    mat))

(defun sum-rows (matrix)
  "Return a column vector with sum of row"
  (let ((mat (make-float-matrix (number-of-rows matrix) 1)))
    (loop for r below (number-of-rows matrix) do
	 (setf (matrix-ref mat r)
	       (loop for c below (number-of-cols matrix) sum
		    (matrix-ref matrix r c))))
    mat))
		    
  
(defun make-diagonal-matrix (affinity)
  (diag (sum-rows affinity)))
	       

(defun bipartite-matrix (W D)
  "Getting the eigenvector with the second smallest eigenvalue. W is affinity matrix, and D is diagonal matrix."
  (let* ((D-inv (m/ D))
	 (A (m* D-inv (m- D W)))) ;; D^-1(D-W)y=ly
    (multiple-value-bind (V E) (eig A :VN)
      ;; Use the eigenvector with the second smallest eigenvalue
      ;; This means the second column of V
      ;; Use 0 to classify the points
      (let ((group-a nil)
	    (group-b nil))
	(loop for r below (number-of-rows E) do
	     (if (> (matrix-ref V r 1) 0)
		 (push r group-a)
		 (push r group-b)))
	(list group-a group-b)))))
	     


		       

    

  

