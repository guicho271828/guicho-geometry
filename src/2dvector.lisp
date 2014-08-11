(in-package :guicho-geometry)

(annot:enable-annot-syntax)

@export
@export-slots
@export-accessors
(defclass 2dvector (deep-copyable)
  ((x :type number :initarg :x :accessor x-of)
   (y :type number :initarg :y :accessor y-of)))

(defmethod print-object ((v 2dvector) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "[~A,~A]"
            (x-of v) (y-of v))))

(defmethod reinitialize-instance :after ((v 2dvector) 
                                         &key original &allow-other-keys)
  (when original
    (setf (x-of v) (x-of original)
          (y-of v) (y-of original))))

(declaim (inline 2dv))

@export
(defun 2dv (x y)
  @type number x y
  (make-instance '2dvector :x x :y y))

@export
(defun make-random-2dv (x0 y0 x1 y1)
  (2dv (random-between x0 x1)
       (random-between y0 y1)))

@export
(defvar +2origin+ (2dv 0.0d0 0.0d0))
@export
(defvar +2ex+ (2dv 1.0d0 0.0d0))
@export
(defvar +2ey+ (2dv 0.0d0 1.0d0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  functions  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@export
@doc "the vector product of 2 vectors.
for 2dvector, it just returns the area of parallelogram
 made by those vectors."
(defun perp-dot (v1 v2)
  @type 2dvector v1
  @type 2dvector v2
  (- (* (x-of v1)
        (y-of v2))
     (* (x-of v2)
        (y-of v1))))

@export
@doc "create a new vector by rotating the original by pi/2 radian"
(defun rotate90 (v)
  @type 2dvector v
  (make-instance (class-of v)
                 :x (- (y-of v))
                 :y (x-of v)))
@export
@doc "create a new vector by rotating the original by -pi/2 radian"
(defun rotate90-clockwise (v)
  @type 2dvector v
  (make-instance (class-of v)
                 :x (y-of v)
                 :y (- (x-of v))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; vector methods

@export
(defun add-vector (v1 v2)
  @type 2dvector v1
  @type 2dvector v2
  (make-instance '2dvector
                 :x (+ (x-of v1) (x-of v2))
                 :y (+ (y-of v1) (y-of v2))))

(defmethod binary-add ((v1 2dvector) (v2 2dvector))
  (add-vector v1 v2))

@export
(defun sub-vector (v1 v2)
  @type 2dvector v1
  @type 2dvector v2
  (make-instance '2dvector
                 :x (- (x-of v1) (x-of v2))
                 :y (- (y-of v1) (y-of v2))))

(defmethod binary-sub ((v1 2dvector) (v2 2dvector))
  (sub-vector v1 v2))

@export
(defun nadd-vector (v-modified v2)
  (with-slots (x y) v-modified
    (setf x (+ x (x-of v2))
          y (+ y (y-of v2)))
    v-modified))

@export
(defun nsub-vector (v-modified v2)
  @type 2dvector v-modified
  @type 2dvector v2
  (with-slots (x y) v-modified
    (setf x (- x (x-of v2))
          y (- y (y-of v2)))
    v-modified))

(defmethod binary-nadd ((v1 2dvector) (v2 2dvector))
  (nadd-vector v1 v2))
(defmethod binary-nsub ((v1 2dvector) (v2 2dvector))
  (nsub-vector v1 v2))

@export
(defun dot-vector (v1 v2)
  @type 2dvector v1
  @type 2dvector v2
  (+ (* (x-of v1) (x-of v2))
     (* (y-of v1) (y-of v2))))

(defmethod dot ((v1 2dvector) (v2 2dvector))
  (dot-vector v1 v2))

@export
(defun norm2-vector (v1)
  @type 2dvector v1
  (+ (d^2 (x-of v1))
     (d^2 (y-of v1))))

(defmethod norm2 ((v1 2dvector))
  (norm2-vector v1))

(defmethod normalize ((v1 2dvector))
  (scale-vector v1 (/ 1.0d0 (norm v1))))


(defmethod resize ((v1 2dvector) (length number))
  (scale-vector v1 (/ length (norm v1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; shape methods

@export
(defun scale-vector (v1 c)
  @type 2dvector v1
  @type number c
  (make-instance '2dvector
                 :x (* c (x-of v1))
                 :y (* c (y-of v1))))

@export
(defun nscale-vector (v1 c)
  @type 2dvector v1
  @type number c
  (with-slots (x y) v1
    (setf x (* c x)
          y (* c y))
    v1))

(defmethod scale ((v1 2dvector) (c number))
  (scale-vector v1 c))

(defmethod translate ((v1 2dvector) (v2 2dvector))
  (add-vector v1 v2))

(defmethod distance ((v1 2dvector) (v2 2dvector))
  (distance-vector v1 v2))

@export
(defun distance-vector (v1 v2)
  @type 2dvector v1
  @type 2dvector v2
  (norm (sub-vector v1 v2)))

(defmethod parallel-p ((v1 2dvector) (v2 2dvector))
  (zerop (perp-dot v1 v2)))

(defmethod perpendicular-p ((v1 2dvector) (v2 2dvector))
  (zerop (dot-vector v1 v2)))

(defmethod congruent-p ((v1 2dvector) (v2 2dvector))
  (and (=~ (x-of v1) (x-of v2))
       (=~ (y-of v1) (y-of v2))))

(defmethod ->list ((v 2dvector))
  (list (x-of v) (y-of v)))

(defmethod dimension ((v 2dvector))
  (make-instance (class-of v) :x 0.0d0 :y 0.0d0))

(defmethod boundary ((v 2dvector))
  (make-instance '2drectangle :bottom-left v :top-right v))

(defmethod angle ((v 2dvector))
  (with-slots (x y) v
    (datan y x)))

(defmethod slope ((v 2dvector))
  (with-slots (x y) v
    (/ y x)))

(defmethod center-of ((v 2dvector)) v)


(defmethod projection-of ((v1 2dvector) (v2 2dvector))
  (scale-vector v2 (/ (dot-vector v1 v2) (norm2-vector v2))))

(defmethod radius ((v 2dvector))
  0.0d0)
