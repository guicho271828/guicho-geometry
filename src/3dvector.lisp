
(in-package :guicho-geometry)
(annot:enable-annot-syntax)

@export
@export-slots
(defclass 3dvector (2dvector)
  ((z :type number :initarg :z)))

(declaim (inline z-of (setf z-of) 3dv))

@export
(defun z-of (v)
  @type 3dvector v
  (dslot-value v 'z))
@export
(defun (setf z-of) (x v)
  @type number x
  @type 3dvector v
  (dsetf (slot-value v 'z) x))

@export
(defun 3dv (x y z)
  @type number x
  @type number y
  @type number z
  (make-instance '3dvector :x x :y y :z z))

@export
(defgeneric without-z (v))
(defmethod without-z ((v 3dvector))
  (2dv (x-of v) (y-of v)))
(defmethod without-z (v)
  v)
@export
(defgeneric nwithout-z (v))
(defmethod nwithout-z ((v 3dvector))
  (change-class v '2dvector))
(defmethod nwithout-z (v)
  v)

@export
(defvar +3origin+ (3dv 0.0d0 0.0d0 0.0d0))
@export
(defvar +3ex+ (3dv 1.0d0 0.0d0 0.0d0))
@export
(defvar +3ey+ (3dv 0.0d0 1.0d0 0.0d0))
@export
(defvar +3ez+ (3dv 0.0d0 0.0d0 1.0d0))

(defmethod sub ((v1 3dvector) (v2 3dvector))
  (3dv (- (x-of v1) (x-of v2))
       (- (y-of v1) (y-of v2))
       (- (z-of v1) (z-of v2))))

(defmethod add ((v1 3dvector) (v2 3dvector))
  (3dv (+ (x-of v1) (x-of v2))
       (+ (y-of v1) (y-of v2))
       (+ (z-of v1) (z-of v2))))

(defmethod nadd ((v1 3dvector) (v2 3dvector))
  (setf (x-of v1) (+ (x-of v1) (x-of v2))
        (y-of v1) (+ (y-of v1) (y-of v2))
        (z-of v1) (+ (z-of v1) (z-of v2)))
  v1)

(defmethod nsub ((v1 3dvector) (v2 3dvector))
  (setf (x-of v1) (- (x-of v1) (x-of v2))
        (y-of v1) (- (y-of v1) (y-of v2))
        (z-of v1) (- (z-of v1) (z-of v2)))
  v1)

(defmethod neg ((v1 3dvector))
  (3dv (- (x-of v1))
       (- (y-of v1))
       (- (z-of v1))))

(defmethod nneg ((v1 3dvector))
  (setf (x-of v1) (- (x-of v1))
        (y-of v1) (- (y-of v1))
        (z-of v1) (- (z-of v1))))

(defmethod distance ((v1 3dvector) (v2 3dvector))
  (let ((dx (- (x-of v2) (x-of v1)))
        (dy (- (y-of v2) (y-of v1)))
        (dz (- (z-of v2) (z-of v1))))
    (sqrt (+ (d^2 dx) (d^2 dy) (d^2 dz)))))

(defmethod dot ((v1 3dvector) (v2 3dvector))
  (+ (* (x-of v1) (x-of v2))
     (* (y-of v1) (y-of v2))
     (* (z-of v1) (z-of v2))))

(define-permutation-methods dot ((v1 3dvector) (c number))
  (3dv (* c (x-of v1)) (* c (y-of v1)) (* c (z-of v1))))

(define-permutation-methods ndot ((v1 3dvector) (c number))
  (setf (x-of v1) (* c (x-of v1))
        (y-of v1) (* c (y-of v1))
        (z-of v1) (* c (z-of v1))))

(defmethod norm ((v1 3dvector))
  (sqrt (+ (d^2 (x-of v1))
            (d^2 (y-of v1))
            (d^2 (z-of v1)))))

(defmethod norm2 ((v1 3dvector))
  (+ (d^2 (x-of v1))
     (d^2 (y-of v1))
     (d^2 (z-of v1))))


(defmethod normalize ((v1 3dvector))
  (dot v1 (/ 1.0d0 (norm v1))))

(defmethod resize ((v1 3dvector) (length float))
  (dot v1 (/ length (norm v1))))

(defmethod nresize ((v1 3dvector) (length float))
  (let ((n (/ length (norm v1))))
    (setf (x-of v1) (* (x-of v1) n)
          (y-of v1) (* (y-of v1) n)
          (z-of v1) (* (z-of v1) n))
    v1))

(defmethod vector-prod ((v1 3dvector) (v2 3dvector))
  (3dv (- (* (y-of v1)
             (z-of v2))
          (* (y-of v2)
             (z-of v1)))
       (- (* (z-of v1)
             (x-of v2))
          (* (z-of v2)
             (x-of v1)))
       (- (* (x-of v1)
             (y-of v2))
          (* (x-of v2)
             (y-of v1)))))

(defmethod distance ((p1 3dvector) (p2 3dvector))
  (norm (sub p1 p2)))

(defmethod parallel-p ((v1 3dvector) (v2 3dvector))
  (zerop (norm (vector-prod v1 v2))))

(defmethod perpendicular-p ((v1 3dvector) (v2 3dvector))
  (zerop (dot v1 v2)))

(defmethod congruent-p ((v1 3dvector) (v2 3dvector))
  (and (d~ (x-of v1) (x-of v2))
       (d~ (y-of v1) (y-of v2))
       (d~ (z-of v1) (z-of v2))))

(defmethod ->list ((v 3dvector))
  (list (x-of v) (y-of v) (z-of v)))

@export
(defun make-random-3dv (x0 y0 z0 x1 y1 z1)
  (3dv (drandom-between x0 x1)
       (drandom-between y0 y1)
       (drandom-between z0 z1)))


(defmethod dimension ((v 3dvector))
  (3dv 0.0d0 0.0d0 0.0d0))

(defmethod boundary ((v 3dvector))
  (make-instance '3drectangle :bottom-left v :top-right v))


