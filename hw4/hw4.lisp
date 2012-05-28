
(defpackage #:hw4
  (:use :cl))

(in-package #:hw4)

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
