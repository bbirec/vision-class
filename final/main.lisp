
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
      :accessor h)))



(defun make-neuron-map (height width n)
  (let ((dim (list (* height n) (* width n))))
    (make-instance 'neuronal-map
		   :arr (make-array dim)
		   :n n
		   :w width
		   :h height)))

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

(defun init-model (width height n data)
  (let ((map (make-neuron-map height width n)))
    (loop for y from 0 below height do
	 (loop for x from 0 below width do
	      (let* ((color-idx (+ x (* width y)))
		     (r (aref data color-idx))
		     (g (aref data (+ color-idx 1)))
		     (b (aref data (+ color-idx 2)))
		     (hsv (rgb->hsv r g b)))
		(loop for i from 0 below (* n n) do
		     (setf (ref-neuron map y x i) hsv)))))
    map))
		     

		
		
		       
    



;; Load the datasets
(defun load-jpeg (path)
  (multiple-value-bind (buffer width height comp) (jpeg:decode-image path)
    buffer))


(defparameter *ep1* 0.3) ;; Learning
(defparameter *ep2* 0.1) ;; BS

(defparameter *alpha1* 0.3)
(defparameter *alpha2* 0.1)

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
    
(defun alpha-function (learning-state frame total-frame)
  (if learning-state
      (- *alpha1* (/ (* (- *alpha1* *alpha2*) frame) total-frame))
      *alpha2*))
		 

	 
(defun update-adjacent-neurons (map y x i p alpha gw)
  "Update the adjacent neurons with the new value"
  (let ((pos (neuron-position map y x i)))
    (loop for y from -1 to 1 append
	 (loop for x from -1 to 1 
	    for real-x = (+ (car pos) x)
	    for real-y = (+ (cadr pos) y)
	    if (and (>= real-x 0)
		    (>= real-y 0)
		    (< real-x (w map))
		    (< real-y (h map)))
	    collect (list real-x real-y)))))


  


(defun learning-update-map (map y x p frame total-frame)
  (let ((i (find-matching map y x p *ep1*))
	(alpha (alpha-function t frame total-frame)))
    (if i
	;; Update A
	

      
	    
	    
	
      

;; Learning
(defun learning (img))

;; Background subtraction
(defun background-substraction (img))

(defparameter *boats-path* #p"/Users/bbirec/Dropbox/Classes/vision/final/dataset/boats/")


(defun run (dataset-folder)
  ;; train folder and test folder
  (let ((train-folder (merge-pathnames "train" dataset-folder))
	(test-folder (merge-pathnames "test" dataset-folder)))
    (let ((train-images (cl-fad:list-directory train-folder))
	  (test-images (cl-fad:list-directory test-folder)))
      
      ;; Init map
      
      ;; learning
      
      ;; background subtraction
      )))
      
  
		 
