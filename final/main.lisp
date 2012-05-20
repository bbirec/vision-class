
(defpackage #:vision-final
  (:use :cl :cl-fad))

(in-package #:vision-final)

;; Implementation of adaptive SOM

(defun rgb->hsv (r g b)
  ;; RGB is 0~1
  (setf r (/ r 256)
	g (/ g 256)
	b (/ b 256))

  (let* ((v (max r g b))
	 (delta (- v (min r g b))))
    (if (or (= v 0) (= delta 0)) ;; in case of r=g=b
	(list -1 0 v)
	(let ((s (/ delta v)))
	  (flet ((get-h ()
		   (if (= r v)
		       (/ (- g b) delta)
		       (if (= g v)
			   (+ 2 (/ (- b r) delta))
			   (+ 4 (/ (- r g) delta))))))
	    (let ((h (* (get-h) 60)))
	      (if (< h 0)
		  (list (+ h 360) s v)
		  (list h s v))))))))

	    
(defun hsv-distance (p1 p2)
  "Euclidean distance of vectors in the HSV color hexcone"
  (flet ((get-vec (h s v) 
	   (list (* v s (cos h))
		 (* v s (sin h))
		 v)))
    (let ((vec1 (apply #'get-vec p1))
	  (vec2 (apply #'get-vec p2)))
      ;; Square of two norm of (vec1 - vec2)
      (reduce #'+ 
	      (mapcar #'(lambda (x) (* x x))
		      (mapcar #'- vec1 vec2))))))

(defun hsv* (alpha p)
  (mapcar #'(lambda (x) (* alpha x)) p))

(defun hsv+ (p1 p2)
  (mapcar #'+ p1 p2))

(defun gaussian-filter (size sigma)
  "Sample the gaussian in array form."
  (assert (oddp size))
  (let ((matrix (make-array (list size size)))
	(diff (floor (/ size 2))))
    (loop for y from 0 below size do
	 (loop for x from 0 below size do
	      (let ((fx (- x diff))
		    (fy (- y diff)))
		(setf (aref matrix x y)
		      (/ (exp (- (/ (+ (* fx fx) (* fy fy))
				    (* 2 sigma sigma))))
			 (* 2 pi sigma sigma))))))
    matrix))


(defclass a-som ()
  (matrix
   ep1
   ep2))



(defclass neuronal-map ()
  ((arr :initarg :arr
	:accessor arr)
   (n :initarg :n
      :accessor n)
   (w :initarg :w
      :accessor w)
   (h :initarg :h
      :accessor h)
   (gaussian :initarg :gaussian
	     :accessor gaussian)))

(defparameter *gaussian-sigma* 1)

(defun make-neuron-map (height width n)
  (let ((dim (list (* height n) (* width n))))
    (make-instance 'neuronal-map
		   :arr (make-array dim)
		   :n n
		   :w width
		   :h height
		   :gaussian (gaussian-filter n *gaussian-sigma*))))

(defun neuron-position (map y x i)
  "Return position in the nueral map"
  (let* ((n (n map))
	 (dx (floor (mod i n)))
	 (dy (floor (/ i n))))
    (list (+ (* y n) dy) 
	  (+ (* x n) dx))))

(defun ref-neuron (map y x i)
  (let ((pos (neuron-position map y x i)))
    (aref (arr map) (car pos) (cadr pos))))

(defun (setf ref-neuron) (value map y x i)
  (let ((pos (neuron-position map y x i)))
    (setf (aref (arr map) (car pos) (cadr pos)) value)))

(defun hsv-pixel (data w y x)
  (let* ((color-idx (+ x (* w y)))
	 (r (aref data color-idx))
	 (g (aref data (+ color-idx 1)))
	 (b (aref data (+ color-idx 2)))
	 (hsv (rgb->hsv r g b)))
    hsv))

(defvar *map* nil)
(defun init-model (width height n data)
  (let ((map (make-neuron-map height width n)))
    (loop for y from 0 below height do
	 (loop for x from 0 below width do
	      (loop for i from 0 below (* n n) do
		   (setf (ref-neuron map y x i) (hsv-pixel data width y x)))))
    (setf *map* map)))
		     
(defmacro ref-neuron-direct (map y x)
  `(aref (arr ,map) ,y ,x))
		
		
(defun show-neuron (map y x)
  (loop for i below (* (n map) (n map)) do
       (format t "Value ~A : ~A~%" i (ref-neuron map y x i))))
    



;; Load the datasets
(defun load-jpeg (path)
  (jpeg:decode-image path))


(defparameter *ep1* 0.3) ;; Learning
(defparameter *ep2* 0.1) ;; BS

(defparameter *alpha1* 0.3)
(defparameter *alpha2* 0.1)

;; Shadow detection parameters
(defparameter *gamma-v* 0.001)
(defparameter *beta-v* 0.1)
(defparameter *tau-s* 0.001)
(defparameter *tau-h* 0.001)

(defun shadow-p (p c)
  (destructuring-bind (p-h p-s p-v) p
    (destructuring-bind (c-h c-s c-v) c
      (and (let ((v (/ p-v c-v)))
	     (and (<= *gamma-v* v)
		  (<= v *beta-v*)))
	   (<= (- p-s c-s) *tau-s*)
	   (<= (abs (- p-h c-h)) *tau-h*)))))


(defun pixel-weights (map y x p)
  "List of weight for the given pixel value"
  (let ((n (n map)))
    (loop for i below (* n n)
       for w = (hsv-distance (ref-neuron map y x i) p)
       collect w)))

(defun find-matching (map y x p ep)
  (let* ((weights (pixel-weights map y x p))
	 (min-weight (loop for w in weights if (< w ep) minimizing w)))
    (position min-weight weights)))
    
(defun find-shadow (map y x p)
  (some #'(lambda (c) (shadow-p p c))
	(loop for i below (* (n map) (n map)) collect
	     (ref-neuron map y x i))))
	     
	 
(defun update-adjacent-neurons (map y x i p alpha)
  "Update the adjacent neurons with the new value"
  (let ((pos (neuron-position map y x i)))
    (loop for y from -1 to 1 
       for yi from 0 do
	 (loop for x from -1 to 1 
	      for xi from 0
	    for real-x = (+ (car pos) x)
	    for real-y = (+ (cadr pos) y)
	    if (and (>= real-x 0)
		    (>= real-y 0)
		    (< real-x (w map))
		    (< real-y (h map)))
	      do 
	      (let ((g-alpha (* alpha (aref (gaussian map) yi xi))))
		(assert (<= g-alpha 1))
		(setf (ref-neuron-direct map real-y real-x)
		      (hsv+ 
		       (hsv* (- 1 g-alpha) (ref-neuron-direct map real-y real-x))
		       (hsv* g-alpha p))))))))
		       


(defun learning-update-map (map y x p frame total-frame)
  (let ((i (find-matching map y x p *ep1*))
	(alpha (- *alpha1* (/ (* (- *alpha1* *alpha2*) frame) total-frame))))
    (if i
	;; Update A
	(update-adjacent-neurons map y x i p alpha))))

;; Result
(defvar *result* nil)
(defun init-result (w h)
  (setf *result* (make-array (list h w))))
(defun set-result (y x value)
  ;; 1 for foreground
  ;; 0 for background
  (setf (aref *result* y x) value))

      

(defun online-update-map (map y x p)
  (let ((i (find-matching map y x p *ep2*))
	(alpha *alpha2*))
    (if i
	(progn
	  (set-result y x 0)
	  (update-adjacent-neurons map y x i p alpha))
	(if (find-shadow map y x p)
	    ;; Background
	    (set-result y x 0)
	    ;; Foreground
	    (set-result y x 1)))))
	    

(defparameter *boats-path* #p"/Users/bbirec/Dropbox/Classes/vision/final/dataset/boats/")


(defun run (dataset-folder)
  ;; train folder and test folder
  (let ((train-folder (merge-pathnames "train" dataset-folder))
	(test-folder (merge-pathnames "test" dataset-folder)))
    (let ((train-images (cl-fad:list-directory train-folder))
	  (test-images (cl-fad:list-directory test-folder)))

      ;; Load the first image in the train images to init the result buffer.
      (multiple-value-bind (b w h c) (load-jpeg (car train-images))
	(assert (= c 3))

	(format t "Found ~A train images and ~A test images.~%"
		(length train-images)
		(length test-images))

	(format t "Initializing the result and the model with the first train image.~%")
	
	(init-result w h)
	(let ((map (init-model w h 3 b))
	      (total-frames (- (length train-images) 1)))

	  ;; Learning with the rest images of the train set.
	  (loop for image-path in train-images 
	       for frame from 0
	       for data = (load-jpeg image-path) do
	       (format t "Learning frame #~A.~%" frame)
	       (loop for y below h do
		    (loop for x below w do
			 (learning-update-map map y x 
					      (hsv-pixel data w y x)
					      frame
					      total-frames))))
	  
	  ;; Online update map with test images
	  #+nil
	  (loop for image-path in test-images 
	       for data = (load-jpeg image-path) do
	       (loop for y below h do
		    (loop for x below w do
			 (online-update-map map y x (hsv-pixel data w y x))))))))))

      
  
		 
