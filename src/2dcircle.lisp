

(in-package :guicho-geometry)
(annot:enable-annot-syntax)
;; 対角線上の二点をスロットに持つ

@export
(defclass 2dcircle (deep-copyable radius-based-mixin 2dshape)
  ((c :type 2dvector :initarg :center :reader center-of)
   (r :type number :initarg :radius :reader radius)))

@export
(defun circle (center radius)
  @type 2dvector center
  @type number radius
  (new '2dcircle :center center :radius radius))

(defmethod print-object ((v 2dcircle) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (r c) v
      (format stream "RADIUS: ~A CENTER: ~A" r c))))

(defmethod dimension ((s 2dcircle))
  (with-slots (c r) s
    (let ((d (* r 2.0d0)))
      (2dv d d))))

(defmethod congruent-p ((s1 2dcircle) (s2 2dcircle))
  (= (radius s1) (radius s2)))

(defmethod boundary ((s 2dcircle))
  (with-slots (c r) s
    (let ((d (2dv r r)))
      (make-instance '2drectangle
                     :bottom-left (sub c d)
                     :top-right (add c d)))))

(defmethod distance ((circle 2dcircle) (v 2dvector))
  (with-slots (c) circle
    (distance c v)))

(defmethod minimum-distance ((circle 2dcircle) (v 2dvector))
  (with-slots (c r) circle
    (- (distance c v) r)))

(defmethod contains-p ((circle 2dcircle) (v 2dvector))
  (minusp (minimum-distance circle v)))

(defmethod intersects-p ((ci1 2dcircle) (ci2 2dcircle))
  (with-slots ((c1 c) (r1 r)) ci1
    (with-slots ((c2 c) (r2 r)) ci2
      (< (distance c1 c2) (+ r1 r2)))))

(define-permutation-methods intersects-p ((circle 2dcircle) (po 2dpolygon))
  (with-slots (c r) circle
    (let ((axes (separating-axes po)))
      (every (lambda (axis)
               (let* ((axis (normalize axis))
                      (projections (mapcar (lambda (v) (dot-vector v axis)) (vertices-of po))))
                 (intersects-p (make-range (reduce #'max projections)
                                           (reduce #'min projections))
                               (let ((projection (dot-vector c axis)))
                                 (make-range (+ projection r)
                                             (- projection r))))))
             axes))))

(defmethod translate ((circle 2dcircle) (v 2dvector))
  (deep-copy circle :center (add (center-of circle) v)))
(defmethod scale ((circle 2dcircle) (n number))
  (deep-copy circle :radius (* (radius circle) n)))
