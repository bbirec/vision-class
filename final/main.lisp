
(defpackage #:vision-final
  (:use :cl :cl-fad))

(in-package #:vision-final)

;; Implementation of adaptive SOM

(defun rgb->hsv (r g b)
  (let* ((v (max r g b))
	 (delta (- v (min r g b))))
    (if (= v 0)
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


(defclass a-som ()
  (matrix
   ep1
   ep2))
   
(defclass neuronal-map ()
  ((arr :initarg :arr
	:accessor arr)
   (n :initarg :n
      :accessor n)))


(defun make-neuron-map (height width n)
  (let ((dim (list (* height n) (* width n))))
    (make-instance 'neuronal-map
		   :arr (make-array dim)
		   :n n)))

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
		     (r (nth color-idx data))
		     (g (nth (+ color-idx 1) data))
		     (b (nth (+ color-idx 2) data))
		     (hsv (rgb->hsv r g b)))
		(loop for i from 0 below (* n n) do
		     (setf (ref-neuron map y x i) hsv)))))
    map))
		     

		
		
		       
    



;; Load the datasets
(defun load-jpeg (path)
  (multiple-value-bind (buffer width height comp) (jpeg:decode-image path)))


(defparameter *ep1* 0.3) ;; Learning
(defparameter *ep2* 0.1) ;; BS

(defun find-matching ())

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
      
  
		 
