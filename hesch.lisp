;;;; Henderson ESCHer
;;;; An OpenGL implementation of Peter Henderson's functional
;;;; language to describe Escher's Square Limit

(defpackage :hesch (:use :cl :cepl :rtg-math :dirt))
(in-package :hesch)

(defvar *tex-path* "/home/userprime/Pictures/sprites/mew400.png")
(defvar *tex* nil)
(defvar *verts* nil)
(defvar *modelm* nil) ;model->world matrix
(defvar *viewm* nil)  ;world->cam matrix
(defvar *projm* nil)  ;cam->clip matrix
(defvar *images* nil) ;list of images to render
(defvar *default-fbo* nil)

(defstruct rect
  (origin #C(0 0))
  (horiz 1)
  (vert 1)
  (rot 0))

(defstruct image
  rect
  tex)

(defun initialize ()
  (setf *modelm* (m4:identity))
  (setf *viewm* (m4:identity))
  (set-texture *tex-path*)
  (setf *verts*
    (make-buffer-stream
       (make-gpu-array `((,(v! 0 1 0) ,(v! 0 1))
			 (,(v! 0 0 0) ,(v! 0 0))
			 (,(v! 1 0 0) ,(v! 1 0))
			 (,(v! 1 0 0) ,(v! 1 0))
			 (,(v! 1 1 0) ,(v! 1 1))
			 (,(v! 0 1 0) ,(v! 0 1)))
		       :dimensions 6 :element-type 'g-pt))))

(defun set-texture (filename)
  (setf *tex* (sample (dirt:load-image-to-texture filename))))

;;; Pipeline---

(defun-g hesch-vert ((vert g-pt)
		     &uniform (modelm :mat4)
		     (viewm :mat4))
  (values (* modelm (v! (pos vert) 1.0))
	  (:smooth (tex vert))))

(defun-g hesch-frag ((tex-coord :vec2)
		     &uniform (tex :sampler-2d))
  (* (texture tex tex-coord) (v! 1 1 1 1)))

(defpipeline-g render-rects ()
  (hesch-vert g-pt)
  (hesch-frag :vec2))

;;;Draw commands---

(defun draw-tex ()
  (clear)
  (map-g #'render-rects *verts*
	 :tex *tex* :modelm *modelm*)
  (swap))

(defun draw-to-screen ()
  (clear)
  (loop for image in *images* do
       (map-g #'render-rects *verts*
	      :tex (image-tex image)
	      :modelm (rect-to-m4 (image-rect image))))
  (swap))

(defun flush-to-screen ()
  (clear)
  (do ((image (pop *images*) (pop *images*)))
      ((null image))
    (map-g #'render-rects *verts*
	   :tex (image-tex image)
	   :modelm (rect-to-m4 (image-rect image))))
  (swap))

(defun draw-to-texture (texture)
  )

;;Rect measurements are in world coords, they will construct the model matrix

(defun rect-to-m4 (rect)
  "Scale, translate middle to origin, rotate, translate back, translate"
  (let ((x (realpart (rect-origin rect)))
	(y (imagpart (rect-origin rect)))
	(w (rect-horiz rect))
	(h (rect-vert rect)))
    (m4:* (m4:translation (v! x y 0))                     ;T
	  (m4:translation (v! (/ w 2) (/ h 2) 0))         ;Tb
	  (m4:rotation-z (coerce (rect-rot rect) 'single-float))
	  (m4:translation (v! (- (/ w 2)) (- (/ h 2)) 0)) ;To
	  (m4:scale (v! w h 0)))))

(defun print-m4 (m4)
  "Pretty print the 4x4 matrix"
  (loop for i from 0 below 4 do
       (loop for j from 0 below 4 do
	    (let ((x (aref m4 (+ j (* i 4)))))
	      (if (< x 0)
		  (format t "~S " x)
		  (format t " ~S " x))))
       (format t "~%")))

;;A picture is a function of rect, it encloses a texture to draw into a rectangle

(defun make-pic (tex)
  "Batches rectangle texture in image for drawing"
  (lambda (rect)
    (push (make-image :rect rect :tex tex) *images*)))

(defun make-pic1 (tex)
  "A pic from this constructor issues a draw call for itself"
  (lambda (rect)
    (map-g #'render-rects *verts*
	   :tex tex
	   :modelm (rect-to-m4 rect))))

;;;Henderson's Basic Operations---

(defun beside (p1 p2 a)
  "Places two pictures besides each other at a normalized scaling value of a"
  (lambda (rect)
    (funcall p1
	     (make-rect :origin
			(complex (realpart (rect-origin rect))
				 (imagpart (rect-origin rect)))
			:horiz (* a (rect-horiz rect))
			:vert (rect-vert rect)
			:rot (rect-rot rect)))
    (funcall p2
	     (make-rect :origin
			(complex (+ (realpart (rect-origin rect))
				    (* a (rect-horiz rect)))
				 (imagpart (rect-origin rect)))
			:horiz (* (- 1 a) (rect-horiz rect))
			:vert (rect-vert rect)
			:rot (rect-rot rect)))))

(defun above (p1 p2 a)
  "Places p2 above p1 according to normalized scaling value a"
  (lambda (rect)
    (funcall p1
	     (make-rect :origin
			(complex (realpart (rect-origin rect))
				 (imagpart (rect-origin rect)))
                        :horiz (rect-horiz rect)
			:vert (* a (rect-vert rect))
			:rot (rect-rot rect)))
    (funcall p2
	     (make-rect :origin
			(complex (realpart (rect-origin rect))
				 (+ (imagpart (rect-origin rect))
				    (* a (rect-vert rect))))
                        :horiz (rect-horiz rect)
			:vert (* (- 1 a) (rect-vert rect))
			:rot (rect-rot rect)))))

(defun grot (p1 a)
  "General anti-clockwise rotation by a"
  (lambda (rect)
    (funcall p1
	     (make-rect :origin
			(complex (realpart (rect-origin rect))
				 (imagpart (rect-origin rect)))
			:horiz (rect-horiz rect)
			:vert (rect-vert rect)
			:rot (+ (rect-rot rect) a)))))

(defun rot (p1)
  "90 degree anti-clockwise rotation"
  (grot p1 (/ pi 2)))

(defun rot45 (p1)
  "45 degree anti-clockwise rotation and TODO:scaling"
  (grot p1 (/ pi 4)))

(defun flip (p1)
  "If this doesn't work, gonna have to fuck w/UVs"
  (lambda (rect)
    (funcall p1
	     (make-rect :origin
			(complex (realpart (+ (rect-origin rect)
					      (rect-horiz rect)))
				 (imagpart (rect-origin rect)))
			:horiz (- (rect-horiz rect))
			:vert (rect-vert rect)
			:rot (rect-rot rect)))))
;;Combinations----

(defun quad (p1)
  (above (beside p1 p1 .5) (beside p1 p1 .5) .5))

(defun test ()
  (set-texture "/home/userprime/Pictures/cool_garfield.jpg")
  (let* ((vp (make-viewport '(1024 768)))
	 (mewp (make-pic *tex*))
	 (rmewp (rot (rot mewp)))
	 (bmewp (beside rmewp rmewp .5))
	 (qmewp (above bmewp bmewp .5))
	 (45mewp (rot45 mewp))
	 (qrmewp (quad 45mewp))
	 (rqrmewp (rot qrmewp)))
    (funcall qmewp (make-rect :origin #C(-1 -1)
			      :horiz 2 :vert 2))
    (funcall qrmewp (make-rect :origin #C(-0.5 -0.5)))
    (funcall rqrmewp (make-rect :origin #C(-0.25 -0.25)
				:horiz 0.5 :vert 0.5))
    (with-viewport vp
      (flush-to-screen))))


