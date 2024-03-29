
(defpackage #:hw4
  (:use :cl))

(in-package #:hw4)

(defparameter *particle-size* 100)
(defparameter *histogram-size* 20)

;; Load PGM images using lispbuilder-sdl-image

(defvar *bmp-path* #p"/Users/bbirec/Dropbox/Classes/vision/hw4/")

(defun surface->pixel-arr (surface)
  "Return a 1D pixel array from the surface"
  
  (let* ((w (sdl:width surface))
	 (h (sdl:height surface))
	 (pixels (make-array (* w h))))

    (sdl-base::with-pixel (pix (sdl:fp surface))
      ;; Support only 1 bpp image
      ;(assert (= 1 (sdl-base::pixel-bpp pix)))
      
      (loop for i below (* w h) do
	   (setf (aref pixels i) 
		 (sdl-base::mem-aref (sdl-base::pixel-data pix) 
				     :unsigned-char i))))
    pixels))

;; Histogram
(defun histogram (pixels)
  (let ((divider (/ 256 *histogram-size*))
	(h (make-array *histogram-size*)))
    (loop for p in pixels do
	 (let ((idx (floor (/ p divider))))
	   (setf (aref h idx)
		 (+ (aref h idx) 1))))
    (coerce h 'list)))
	 
(defun histogram-rect (image-pixel-arr image-width rect)
  (let ((x (caar rect))
	(y (cadar rect))
	(w (- (caadr rect) (caar rect)))
	(h (- (cadadr rect) (cadar rect))))
    (histogram 
     (loop for j from y below (+ y h) append
	  (loop for i from x below (+ x w) collect
	       (aref image-pixel-arr (+ (* image-width j) i)))))))



(defvar *ref-histogram* nil)
(defparameter *histogram-lambda* 20)

(defun distance-histogram (h1 h2)
  (assert (= (length h1) (length h2)))
  (exp 
   (reduce #'+ 
	   (mapcar #'(lambda (x y) (* (sqrt (* x y)) *histogram-lambda*))
		   h1 h2))))


;; Gaussian Sampling
(defun gaussian-filter (size sigma)
  "Sample the gaussian in array form."
  (assert (oddp size))
  (let ((matrix (make-array size))
	(diff (floor (/ size 2))))
    (loop for x from 0 below size do
	 (let ((fx (- x diff)))
	   (setf (aref matrix x)
		 (/ (exp (- (/ (* fx fx)
			       (* 2 sigma sigma))))
		    (sqrt (* 2 pi sigma sigma))))))
    ;; Normalize
    (let* ((samples (coerce matrix 'list))
	   (total (reduce #'+ samples)))
      (mapcar #'(lambda (x) (/ x total))
	      samples))))

(defun gaussian-sample (size k sigma)
  (mapcar #'(lambda (x) (round (* k x)))
	  (gaussian-filter (+ (* size 2) 1) sigma)))

(defparameter *gaussian-weight* '(1 3 7 12 18 20 18 12 7 3 1))

(defun random-from-to (from to)
  (if (= from to) 
      from
      (+ (random (- to from)) from)))

(defun gaussian-random-bound (bound)
  (let ((values (loop for i from (- (/ bound 2)) to (/ bound 2)
		   by (/ bound (length *gaussian-weight*)) collect i)))

    (loop for i below (- (length values) 1) append
	 (let ((i1 (floor (nth i values)))
	       (i2 (floor (nth (+ i 1) values))))

	   (loop for j below (nth i *gaussian-weight*) collect
		(random-from-to i1 i2))))))

(defun seqrnd (seq)
  "Randomize the elements of a sequence. Destructive on SEQ."
  (sort seq #'> :key (lambda (x) (random 1.0))))
		      
(defun random-rect-with-gaussian (rect diff)
  "Generating random K rects"
  (let* ((ul (car rect))
	 (lr (cadr rect))
	 (bounds1 (seqrnd (gaussian-random-bound diff)))
	 (bounds2 (seqrnd (gaussian-random-bound diff)))
	 (bounds3 (seqrnd (gaussian-random-bound diff)))
	 (bounds4 (seqrnd (gaussian-random-bound diff))))
    (mapcar #'list 
	    (loop for dx in bounds1
	       for dy in bounds2 collect
	       (list (+ (car ul) dx) (+ (cadr ul) dy)))
	    (loop for dx in bounds3
	       for dy in bounds4 collect
	       (list (+ (car lr) dx) (+ (cadr lr) dy))))))

(defun rect-center (rect)
  (let ((ul (car rect))
	(lr (cadr rect)))
    (list (/ (+ (car ul) (car lr)) 2)
	  (/ (+ (cadr ul) (cadr lr)) 2))))

(defun r-size (rect)
  (let ((ul (car rect))
	(lr (cadr rect)))
    (values (- (car lr) (car ul))
	    (- (cadr lr) (cadr ul)))))

(defun r-scale (rect xs ys)
  (multiple-value-bind (w h) (r-size rect)
    (let ((ul (car rect))
	  (lr (cadr rect))
	  (dy (floor (* (- ys 1) h)))
	  (dx (floor (* (- xs 1) w))))
      (list (list (- (car ul) dx)
		  (- (cadr ul) dy))
	    (list (+ (car lr) dx)
		  (+ (cadr lr) dy))))))
      

(defun r-move (rect dx dy)
  (let ((ul (car rect))
	(lr (cadr rect)))
    (list (list (+ (car ul) dx)
		(+ (cadr ul) dy))
	  (list (+ (car lr) dx)
		(+ (cadr lr) dy)))))

(defun r-bound (rect bound)
  (list (mapcar #'max (car rect) (car bound))
	(mapcar #'min (cadr rect) (cadr bound))))

(defun r-area (r)
  (if (null r) 0
      (multiple-value-bind (w h) (r-size r)
	(* w h))))

(defun r-intersection (r1 r2)
  (destructuring-bind ((l1 t1) (r1 b1)) r1
    (destructuring-bind ((l2 t2) (r2 b2)) r2
      (if (not (or (> l2 r1)
		   (< r2 l1)
		   (> t2 b1)
		   (< b2 t1)))
	  (make-rect (max l1 l2)
		     (max t1 t2)
		     (min r1 r2)
		     (min b1 b2))
	  nil))))

(defun random-rect (rect diff)
  (let* ((bounds1 (seqrnd (gaussian-random-bound diff)))
	 (bounds2 (seqrnd (gaussian-random-bound diff)))
	 (bounds3 (seqrnd 
		   (mapcar #'(lambda (x) (+ 1 (/ x (* diff 2))))
			   (gaussian-random-bound diff)))))
    (loop for dx in bounds1 
	 for dy in bounds2
	 for s in bounds3 collect
	 (r-bound (r-scale (r-move rect dx dy) 1 1)
		  '((0 0) (720 480))))))
	 
    
    
    
	 
    
(defvar *ref-rect* nil)
(defvar *cur-rect* nil)

(defvar *rects* nil)

(defvar *random-rects* nil)

(defvar *cur-position* nil)


(defvar *image-idx* 0)
(defparameter *image-paths* nil)


(defvar *image* nil)
(defvar *image-pixels* nil)

(defparameter *ground-truth* nil)

(defvar *auto-load* nil)
(defvar *overlapping-ratio* nil)

(defun load-image (idx)
  (setf *image-idx* idx
	*image* (sdl-image:load-image (nth idx *image-paths*))
	*image-pixels* (surface->pixel-arr *image*)))

(defun draw-rect (r color)
  (sdl:draw-rectangle-* (caar r)
			(cadar r)
			(- (caadr r) (caar r))
			(- (cadadr r) (cadar r))
			:color color))

(defun draw ()
  (if *image*
    (sdl:draw-surface-at *image* (sdl:point :x 0 :y 0)))

  ;; Draw random rects
  (loop for r in *random-rects* do
       (draw-rect r sdl:*yellow*))

  ;; Draw current rect
  (when *cur-rect* 
    (draw-rect *cur-rect* sdl:*white*))

  ;; Draw the ground truth
  (draw-rect (nth *image-idx* *ground-truth*) sdl:*red*))
  

(defun load-next-image ()
  (when (< *image-idx* (- (length *image-paths*) 1))
    (setf *image-idx* (+ 1 *image-idx*))
    (format t "Load ~A image.~%" *image-idx*)
    (load-image *image-idx*)

    ;; Generate the next rect
    (setf *random-rects* (random-rect *cur-rect* 50))
  
    ;; set rect
    (push *cur-rect* *rects*)

    ;; Find best rect
    (let ((hs (loop for r in *random-rects* collect
		   (distance-histogram *ref-histogram*
				       (histogram-rect *image-pixels* 720 r)))))
      (setf *cur-rect* (nth (position (reduce #'max hs) hs) *random-rects*)))

    ;; Calculate the overlapping ratio
    (let* ((tr (nth *image-idx* *ground-truth*))
	   (ratio (* (/ (r-area (r-intersection tr *cur-rect*))
		    (r-area *cur-rect*))
		 100)))
      (setf *overlapping-ratio* (append *overlapping-ratio* (list ratio)))
      (format t "Overlapping ratio: ~3$%~%" ratio))))
    

(defun init ()
  (setf *image-idx* 0
	*image* nil
	*image-pixels* nil
	*ref-rect* nil
	*ref-histogram* nil
	*cur-rect* nil
	*cur-position* nil
	*rects* nil
	*random-rects* nil
	*auto-load* nil
	*overlapping-ratio* nil))
 


(defun parse-string-to-floats (string)
  (let ((*read-eval* nil))
    (with-input-from-string (stream string)
      (loop for number = (read stream nil nil)
            while number collect number))))

(defun make-rect (x1 y1 x2 y2)
  (list (list y1 x1) (list y2 x2)))

(defun load-ground-truth ()
  (setf *ground-truth*
	(with-open-file (s "/Users/bbirec/Dropbox/Classes/vision/hw4/dudek_gt.txt")
	  (loop for line = (read-line s nil 'eof)
	     until (eq line 'eof)
	     collect (apply #'make-rect (parse-string-to-floats line))))))

  

  

(defun main ()
  (sdl:load-library)
  (sdl:with-init ()
    (sdl:window 720 480 :title-caption "HW4" :icon-caption "IMAGE-EXAMPLE")
    (setf (sdl:frame-rate) 30)
    (sdl:initialise-default-font)
    ;; SDL_Image 1.2.8 and later allows the jpg, png and tif
    ;; libraries to be preloaded to speed subsequent
    ;; loading of these images.
    (sdl-image:init-image :jpg :png :tif)

    (init)

    ;; Load the ground truth
    (load-ground-truth)

    ;; Load image pathnames
    (let ((images (cl-fad:list-directory 
		   "/Users/bbirec/Dropbox/Classes/vision/hw4/data/")))
      (setf *image-paths* images
	    *image-idx* 0
	    *image* nil
	    *image-pixels* nil))

    (load-image 0)


    (sdl:update-display)
      
    (sdl:with-events ()
      (:quit-event ()
		   ;; SDL_Image 1.2.8 and later requires a corresponding
		   ;; sdl-image:quit *if* sdl-image:init is used.
		   (sdl-image:quit-image)
		   t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :SDL-KEY-ESCAPE)
			   (sdl:push-quit-event))
		       (if (sdl:key= key :SDL-KEY-SPACE)
			   ;; Load next image
			   (load-next-image))
		       (if (sdl:key= key :SDL-KEY-RETURN)
			   ;; Load next image continuously
			   (setf *auto-load* t)))

      (:mouse-button-down-event (:x x :y y)
				(when (null *ref-rect*)
				  (format t "X:~A Y:~A~%" x y)
				  (setf *cur-position* (list x y))
				  (sdl:update-display)))
      (:mouse-button-up-event (:x x :y y)
			      (when (null *ref-rect*)
				(format t "X:~A Y:~A~%" x y)

				(let* ((rect (list *cur-position* (list x y)))
				       (h (histogram-rect *image-pixels*
							  720
							  rect)))
				  ;; Set reference rect
				  (setf *ref-rect* rect
					*ref-histogram* h
					*cur-rect* rect))

				(sdl:update-display)))

      (:idle ()
	     (sdl:clear-display sdl:*black*)
	     
	     (if *auto-load*
		 (load-next-image))
	     
	     (draw)
	     (sdl:update-display))

      (:video-expose-event () (sdl:update-display)))))
