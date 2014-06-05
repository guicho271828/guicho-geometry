(in-package :guicho-geometry)
(annot:enable-annot-syntax)


@export
@export-accessors
(defclass time-mixin ()
  ((time :type number
         :initarg :t
         :initarg :time)))

(declaim (inline t-of (setf t-of) 2+1dv))
@export
(defun t-of (v)
  @type time-mixin v
  (dslot-value v 'time))
@export
(defun (setf t-of) (x v)
  @type number x
  @type time-mixin v
  (dsetf (slot-value v 'time) x))

@export
(defclass 2+1dvector (time-mixin 2dvector)
  ())

(defmethod print-object ((v 2+1dvector) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "[~a,~a,~a]"
            (x-of v) (y-of v) (t-of v))))

@export
(defun 2dv-2+1dv (v time)
  @type number time
  (2+1dv (x-of v) (y-of v) time))

@export
(defun 2+1dv (x y time)
  @type number x
  @type number y
  @type number time
  (make-instance '2+1dvector :x x :y y :t time))

@export
(defun make-random-2+1dv (x0 y0 x1 y1 t0 t1)
  (2+1dv (drandom-between x0 x1)
         (drandom-between y0 y1)
         (drandom-between t0 t1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; functions

@export
(defun 2+1dv-velocity (2+1dv)
  (with-slots (x y time) 2+1dv
    (2dv (d/ x time) (d/ y time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; vector methods


@export
(defun add-2+1dvector (v1 v2)
  @type 2+1dvector v1
  @type 2+1dvector v2
  (make-instance '2+1dvector
                 :x (+ (x-of v1) (x-of v2))
                 :y (+ (y-of v1) (y-of v2))
                 :t (+ (t-of v1) (t-of v2))))

(defmethod add ((v1 2+1dvector) (v2 2+1dvector))
  (add-2+1dvector v1 v2))

@export
(defun sub-2+1dvector (v1 v2)
  @type 2+1dvector v1
  @type 2+1dvector v2
  (make-instance '2+1dvector
                 :x (- (x-of v1) (x-of v2))
                 :y (- (y-of v1) (y-of v2))
                 :t (- (t-of v1) (t-of v2))))

(defmethod sub ((v1 2+1dvector) (v2 2+1dvector))
  (sub-2+1dvector v1 v2))

@export
(defun nsub-2+1dvector (v-modified v2)
  @type 2+1dvector v-modified
  @type 2+1dvector v2
  (with-accessors ((x1 x-of) (y1 y-of) (t1 t-of)) v-modified
    (with-accessors ((x2 x-of) (y2 y-of) (t2 t-of)) v2
      (setf x1 (- x1 x2)
            y1 (- y1 y2)
            t1 (- t1 t2))))
  v-modified)

@export
(defun nadd-2+1dvector (v-modified v2)
  @type 2+1dvector v-modified
  @type 2+1dvector v2
  (with-accessors ((x1 x-of) (y1 y-of) (t1 t-of)) v-modified
    (with-accessors ((x2 x-of) (y2 y-of) (t2 t-of)) v2
      (setf x1 (+ x1 x2)
            y1 (+ y1 y2)
            t1 (+ t1 t2))))
  v-modified)

@export
(defun dot-2+1dvector (v1 v2)
  @type 2+1dvector v1
  @type 2+1dvector v2
  (+ (* (x-of v1) (x-of v2))
      (* (y-of v1) (y-of v2))
      (* (t-of v1) (t-of v2))))

(defmethod dot ((v1 2+1dvector) (v2 2+1dvector))
  (dot-2+1dvector v1 v2))


@export
(defun norm2-2+1dvector (v1)
  @type 2+1dvector v1
  (+ (d^2 (x-of v1))
      (d^2 (y-of v1))
      (d^2 (t-of v1))))

(defmethod norm2 ((v1 2+1dvector))
  (norm2-2+1dvector v1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; shape methods

@export
(defun scale-2+1dvector (v1 c)
  @type 2+1dvector v1
  @type number c
  (make-instance '2+1dvector
                 :x (* c (x-of v1))
                 :y (* c (y-of v1))
                 :t (* c (t-of v1))))

@export
(defun nscale-2+1dvector (v1 c)
  @type 2+1dvector v1
  @type number c
  (with-slots (x y time) v1
    (setf x (* c x)
          y (* c y)
          time (* c time))
    v1))

(defmethod scale :around ((v1 2+1dvector) (c double-float))
  (scale-2+1dvector v1 c))

(defmethod normalize ((v1 2+1dvector))
  (scale-2+1dvector v1 (d/ 1.0d0 (norm v1))))

(defmethod translate ((v1 2+1dvector) (v2 2+1dvector))
  (add-2+1dvector v1 v2))

(defmethod translate ((v1 time-mixin) (v2 time-mixin))
  (reinitialize-instance
   (call-next-method)
   :t (+ (t-of v1) (t-of v2))))

(defmethod dimension ((v1 time-mixin))
  (change-class (call-next-method)
                (class-of v1)
                :time 0.0d0))

@export
(defgeneric without-t (v))
(defmethod without-t ((v 2+1dvector))
  (with-slots (x y) v
    (2dv x y)))
(defmethod without-t (v)
  v)

