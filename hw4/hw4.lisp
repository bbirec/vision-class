
(defpackage #:hw4
  (:use :cl))

(in-package #:hw4)

(defparameter *particle-size* 100)
(defparameter *histogram-size* 10)

;; Load PGM images using lispbuilder-sdl-image

(defvar *bmp-path* #p"/Users/bbirec/Dropbox/Classes/vision/hw4/")

(defun surface->pixel-arr (surface)
  "Return a 1D pixel array from the surface"
  
  (let* ((w (sdl:width surface))
	 (h (sdl:height surface))
	 (pixels (make-array (* w h))))

    (sdl-base::with-pixel (pix (sdl:fp surface))
      ;; Support only 1 bpp image
      (assert (= 1 (sdl-base::pixel-bpp pix)))
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
	 
(defun histogram-rect (image-pixel-arr image-width x y w h)
  (histogram 
   (loop for j from y below (+ y h) append
	(loop for i from x below (+ x w) collect
	     (aref image-pixel-arr (+ (* image-width j) i))))))



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

    (let* ((image 
	    (sdl-image:load-image (merge-pathnames "test.pgm" *bmp-path*)))
	   (pixels (surface->pixel-arr image)))

      (format t "Pixel: ~A" (array-dimensions pixels))
      (sdl:draw-surface-at image (sdl:point :x 0 :y 0)))
      
    (sdl:update-display)
      
    (sdl:with-events ()
      (:quit-event ()
		   ;; SDL_Image 1.2.8 and later requires a corresponding
		   ;; sdl-image:quit *if* sdl-image:init is used.
		   (sdl-image:quit-image)
		   t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :SDL-KEY-ESCAPE)
			   (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display)))))
