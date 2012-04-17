
(in-package #:hw2)

;; Slime

(defun slime-conn ()
  (let ((connection
       (or swank::*emacs-connection* (swank::default-connection))))
  (when (and connection (not (eql swank:*communication-style* :spawn)))
    (swank::handle-requests connection t))))


;; Matlisp

(defmacro join* (position &rest mats)
  (reduce #'(lambda (a b) `(join ,a ,b ,position)) mats))

(defun rgb->grayscale (img)
  (m./ (m.+ (m.+ (car img) (cadr img)) (caddr img)) 3))


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

(defun 2D->3D-transfom (2d-mat)
  "Convert the 2D transform matrix to 3D transform matrix"
  (assert (and (= (car (size 2d-mat)) 3)
	       (= (cadr (size 2d-mat)) 3)))
  (let ((3d-mat (make-float-matrix 4 4)))
    (setf (matrix-ref 3d-mat 0 0) (matrix-ref 2d-mat 0 0))
    (setf (matrix-ref 3d-mat 1 0) (matrix-ref 2d-mat 1 0))
    (setf (matrix-ref 3d-mat 0 1) (matrix-ref 2d-mat 0 1))
    (setf (matrix-ref 3d-mat 1 1) (matrix-ref 2d-mat 1 1))
    (setf (matrix-ref 3d-mat 3 0) (matrix-ref 2d-mat 2 0))
    (setf (matrix-ref 3d-mat 3 1) (matrix-ref 2d-mat 2 1))
    (setf (matrix-ref 3d-mat 0 3) (matrix-ref 2d-mat 0 2))
    (setf (matrix-ref 3d-mat 1 3) (matrix-ref 2d-mat 1 2))
    (setf (matrix-ref 3d-mat 3 3) (matrix-ref 2d-mat 2 2))
    
    (setf (matrix-ref 3d-mat 2 2) 1)
    3d-mat))

(defun 3D-transform->arr (mat)
  (convert-to-lisp-array (reshape mat 16 1)))

(defun normalize-vector (vec)
  (m/ vec (matrix-ref vec (- (car (size vec)) 1))))

(defun normalize-matrix (mat)
  (assert (square-matrix-p mat))
  (let ((s (matrix-ref mat 
		       (- (car (size mat)) 1)
		       (- (car (size mat)) 1))))
    (m/ mat s)))


;; Image read/write

(defun load-image-from-file (filepath)
  "Converting the input image to matrix form"
  (let* ((img (read-png-file filepath))
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


(defun write-image-to-file (filepath mat-R mat-G mat-B)
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
    (zpng:write-png png filepath)))
