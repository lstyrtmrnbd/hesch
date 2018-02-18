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

;;A picture is a function of rect, it encloses a texture to draw into a rectangle

(defun make-pic (tex)
  "Batches rectangle texture pairs for drawing"
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
			:vert (rect-vert rect)))
    (funcall p2
	     (make-rect :origin
			(complex (+ (realpart (rect-origin rect))
				    (* a (rect-horiz rect)))
				 (imagpart (rect-origin rect)))
			:horiz (* (- 1 a) (rect-horiz rect))
			:vert (rect-vert rect)))))

(defun above (p1 p2 a)
  "Places one picture above another according to normalized scaling value a"
  (lambda (rect)
    (funcall p1
	     (make-rect :origin
			(complex (realpart (rect-origin rect))
				 (imagpart (rect-origin rect)))
                        :horiz (rect-horiz rect)
			:vert (* a (rect-vert rect))))
    (funcall p2
	     (make-rect :origin
			(complex (realpart (rect-origin rect))
				 (+ (imagpart (rect-origin rect))
				    (* a (rect-vert rect))))
                        :horiz (rect-horiz rect)
			:vert (* (- 1 a) (rect-vert rect))))))

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

;;Combinations----


