(defpackage #:hw1
  (:use #:cl #:png-read #:lisplab))

(in-package #:hw1)

(defparameter *path* "/Users/bbirec/Desktop/Classes/vision/hw1/")
(defvar *mixed* nil)
(defvar *img1* nil)
(defvar *img2* nil)
(defvar *img1-half* nil)
(defvar *img2-half* nil)

;;; PNG read and write

(defun read-images (filenames)
  (loop for f in filenames collect
       (read-png-file (concatenate 'string *path* f))))


(defun write-png-file (filename matrix)
  (let* ((w (cols matrix))
	 (h (rows matrix))
	 (png (make-instance 'zpng:png 
			     :color-type :grayscale
			     :width w
			     :height h))
	 (image (zpng:data-array png)))
    (labels ((clamp (x min max)
	       (if (< x min) min
		   (if (> x max) max x))))

      (loop for j from 0 below h do
	   (loop for i from 0 below w do
		(setf (aref image j i 0) 
		      (round (clamp (mref matrix j i) 0.0 255.0)))))
      (zpng:write-png png (concatenate 'string *path* filename)))))

;; Resize image


(defun sub-img (img x y w h)
  (let* ((data (image-data img))
	 (result (make-array (list w h))))
    (loop for j from 0 below h do
	 (loop for i from 0 below w do
	      (setf (aref result i j) (aref data (+ i x) (+ j y)))))
    result))


(defun resize-img (filename img x y w h)
  "Resize IMG with the bound box and save it."
  (let ((data (sub-img img x y w h))
	(mat (dnew 0 h w)))
    (loop for j from 0 below h do
	 (loop for i from 0 below w do
	      (setf (mref mat j i) (aref data i j))))
    (write-png-file filename mat)))

	


;;; Matrix reshape

(defun arr->mat (arr)
  ;; Array check routine.
  ;; The array should be a 2d array
  (let* ((r (length arr))
	 (c (length (car arr)))
	 (mat (dnew 0 r c)))
    (labels ((copy-row (arr row)
	       (loop for a in arr 
		  for i from 0 below c do 
		    (setf (mref mat row i) a))))
      (loop for a in arr
	   for j from 0 below r do
	   (copy-row a j)))
    mat))

(defun cmat->arr (cmat)
  "Convert column matrix to a list form"
  (loop for i from 0 below (rows cmat) collect
       (mref cmat i 0)))

(defun cmat->mat (cmat w h)
  (assert (= (* w h) (rows cmat)))
  (let ((mat (dnew 0 h w)))
    (dotimes (r h mat)
      (dotimes (c w)
	(setf (mref mat r c) (mref cmat (+ (* r w) c) 0))))))

;;; Pixel position operation

(defmacro do-pixels (w h padding (x y) &body body)
  "Iterate all of pixels with binding variables X and Y."
  `(loop for j from ,padding below (- ,h ,padding) do
	(loop for i from ,padding below (- ,w ,padding) do
	     (let ((,y j)
		   (,x i))
	       ,@body))))

(defun find-adjacent-points (x y)
  "Return all of adjacent positions in a list"
  (loop for j from -1 to 1 append
       (loop for i from -1 to 1 collect
	    (list (+ x i) (+ y j)))))

;;; Linear eq

(defun solve-eq (A b)
  "Solving linear equation Ax=b"
  (if (= (rows A) (cols A))
      (m* (minv A) b)
      (let ((A-tp (mtp A)))
	(m* (m* (minv (m* A-tp A)) A-tp) b))))


;; Finding filter
;; => [Average image][h]=[I1*h/2 + I2*h/2]

(defun find-source (x y)
  (let ((data (image-data *mixed*))
	(values nil))
    (loop for j from -1 to 1 do
	 (loop for i from -1 to 1 do
	      (setf values (cons (aref data (+ x i) (+ y j)) values))))
    (reverse values)))


(defun filtered-value (x y)
  (let ((i1 (image-data *img1*))
	(i2 (image-data *img2*)))
    (/ (+ (aref i1 x y) (aref i2 x y)) 2.0)))

	      
(defun find-filter ()
  (let* ((w (width *mixed*))
	 (h (height *mixed*))
	 (points
	  (loop for j from 1 below (- h 1) append
	       (loop for i from 1 below (- w 1) collect
		    (list i j))))
	 (source (arr->mat 
		    (loop for (x y) in points collect
			 (find-source x y))))
	 (target (arr->mat 
		  (loop for (x y) in points collect
		       (list (filtered-value x y))))))
    ;; Least square solution
    (solve-eq source target)))




;; Reconstruction of original image
;; => [filter or zero][Original image vector]=[I1]

(defun reconstruction (img filter)
  (let* ((w (width img))
	 (h (height img))
	 (data (image-data img))
	 (w-2 (- w 2))
	 (h-2 (- h 2))
	 (size (* w h))
	 (size-2 (* w-2 h-2))
	 (src (dnew 0 size-2 size))
	 (dst (dnew 0 size-2)))

    (do-pixels w h 1 (x y)
      (let ((row (+ (* (- y 1) w-2) (- x 1))))
	;; Fill src      
	(loop for (sx sy) in (find-adjacent-points x y)
	   for f in filter do
	     (let ((col (+ (* sy w) sx)))
	       (setf (mref src row col) f)))

	;; Fill dst
	(setf (mref dst row 0) (aref data x y))))

    ;; Solve least square solution
    (let* ((src-tp (mtp src))
	   (src-tp-inv (minv (m* src-tp src)))
	   (q (m* src-tp-inv src-tp))
	   (solution (m* q dst)))

      (write-png-file "verification.png" (cmat->mat (m* src solution) w-2 h-2))
      (write-png-file "original.png" (cmat->mat dst w-2 h-2))
      
      (list solution w h))))
	   

      
	  

(defvar *h* nil)
(defvar *original-1* nil)
(defvar *original-2* nil)


(defun main ()
  (destructuring-bind (mixed img1 img2 img1-half img2-half)
      (read-images '("car_eagle.png" 
		     "car_filtered.png" 
		     "eagle_filtered.png"
		     "car_filtered_part.png"
		     "eagle_filtered_part.png"))
    (setf *mixed* mixed
	  *img1* img1
	  *img2* img2
	  *img1-half* img1-half
	  *img2-half* img2-half)))


	   
(defun result ()
  (let* ((h (find-filter))
	 (a (cmat->arr h)))
    (format t "Question a : ~A" a)

    
    ;; Reconstruct img1
    (destructuring-bind (img w h) (reconstruction *img1-half* a)
      (setf *original-1* img)
      (write-png-file "output1.png" (cmat->mat img w h)))))
	