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
(defvar *pics* nil)   ;list of pictures to render
(defvar *images* nil) ;list of images to render
(defvar *default-fbo* nil)

(defstruct rect
  (origin #C(0 0))
  (vert 1)
  (horiz 1)
  (rot 0))

(defstruct image
  rect
  tex)

(defun initialize ()
  (setf *modelm* (m4:identity))
  (setf *viewm* (m4:identity))
  (setf *tex* (sample (dirt:load-image-to-texture *tex-path*)))
  (setf *verts*
    (make-buffer-stream
       (make-gpu-array `((,(v! 0 1 0) ,(v! 0 1))
			 (,(v! 0 0 0) ,(v! 0 0))
			 (,(v! 1 0 0) ,(v! 1 0))
			 (,(v! 1 0 0) ,(v! 1 0))
			 (,(v! 1 1 0) ,(v! 1 1))
			 (,(v! 0 1 0) ,(v! 0 1)))
		       :dimensions 6 :element-type 'g-pt))))

;;; Pipeline---
(defun-g hesch-vert ((vert g-pt)
		     &uniform (modelm :mat4)
		     (viewm :mat4))
  (values (* modelm (v! (pos vert) 1.0))
	  (:smooth (tex vert))))

(defun-g hesch-frag ((tex-coord :vec2)
		     &uniform (tex :sampler-2d))
  (* (texture tex tex-coord) (v! 0.25 0.4 0.25 1.0)))

(defpipeline-g render-rects ()
  (hesch-vert g-pt)
  (hesch-frag :vec2))

(defun draw ()
  (clear)
  (map-g #'render-rects *verts*
	 :tex *tex* :modelm *modelm*)
  (swap))

(defun draw-screen ()
  (clear)
  (loop for picture in *pics* do
       (map-g #'render-rects *verts*
	      :tex (cdr picture)
	      :modelm (car picture)))
  (swap))

(defun draw-images ()
  (clear)
  (loop for image in *images* do
       (map-g #'render-rects *verts*
	      :tex (image-tex image)
	      :modelm (rect-to-m4 (image-rect image))))
  (swap))

(defun draw-texture (texture)
  )

;;measurements are ostensibly provided in world coords,
;;because this is effectively the model matrix
(defun old-rect (x y w h &optional angle)
  "Makes a matrix that will (optionally rotate) scale then translate the standard square to the size and origin provided"
  (m4:* (m4:translation (v! x y 0))
	(if (null angle)
	    (m4:identity)
	    (m4:rotation-z (coerce angle 'single-float)))
	(m4:scale (v! w h 0))))

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

;;selectors
#||
(defun origin (rect)
  (m4:*v rect
	 (v! 0 0 0 1)))

(defun vert (rect)
  (v:y (m4:*v rect
	      (v! 1 1 1 0))))

(defun horiz (rect)
  (v:x (m4:*v rect
	      (v! 1 1 1 0))))
||#

;;picture is a function of rect,
;;it draws a texture into a rectangle
(defun make-pic0 (tex)
  "Unfinished constructor that batches draw calls"
  (lambda (rect)
    (cons rect tex)))

(defun make-pic1 (tex)
  "A pic from this constructor issues a draw call for itself"
  (lambda (rect)
    (map-g #'render-rects *verts*
	   :tex tex
	   :modelm rect)))

(defun make-pic2 (tex)
  "A constructor which also returns rect for debugging"
  (lambda (rect)
    (map-g #'render-rects *verts*
	   :tex tex
	   :modelm rect)
    rect))

(defun make-pic (tex)
  (lambda (rect)
    (push (make-image :rect rect :tex tex) *images*)))

;;Henderson's Basic Operations----

(defun beside (p1 p2 a)
  "Places two pictures besides each other at a normalized scaling value of a"
  (lambda (rect)
    (funcall p1
	     (make-rect (v:x (origin rect))
			(v:y (origin rect))
			(* a (horiz rect))
			(vert rect)))
    (funcall p2
	     (make-rect (+ (v:x (origin rect))
			   (* a (horiz rect)))
			(v:y (origin rect))
			(* (- 1 a) (horiz rect))
			(vert rect)))))

(defun above (p1 p2 a)
  "Places one picture above another according to normalized scaling value a"
  (lambda (rect)
    (funcall p1
	     (make-rect (v:x (origin rect))
			(v:y (origin rect))
			(horiz rect)
			(* a (vert rect))))
    (funcall p2
	     (make-rect (v:x (origin rect))
			(+ (v:y (origin rect))
			   (* a (vert rect)))
			(horiz rect)
			(* (- 1 a) (vert rect))))))

(defun grot (p1 a)
  "General anti-clockwise rotation by a"
  (lambda (rect)
    (funcall p1 (make-rect (v:x (origin rect))
			   (v:y (origin rect))
			   (horiz rect)
			   (vert rect)
			   a))))

(defun rot (p1)
  "A 90 degree anti-clockwise rotation"
  (lambda (rect)
    (funcall p1 (make-rect (+ (v:x (origin rect))
			      (horiz rect))
			   (v:y (origin rect))
			   (horiz rect)
			   (vert rect)
			   (/ pi 2)))))

(defun nrot (p1)
  "Rot function via new rect struct"
  (lambda (rect)
    (funcall p1
	     (make-rect :origin
			(complex (realpart (rect-origin rect))
				 (imagpart (rect-origin rect)))
			:horiz (rect-horiz rect)
			:vert (rect-vert rect)
			:rot (+ (rect-rot rect) (/ pi 2))))))
;;Combinations----


