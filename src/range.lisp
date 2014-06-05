(in-package :guicho-geometry)
(annot:enable-annot-syntax)

;; @export
;; (deftype range ()
;;   `(simple-array number 2))
;; (defun range (from to)
;;   (let ((r (make-array 2 :element-type 'number)))
;; 	(setf (aref range 0) (coerce (min from to) 'number)
;; 		  (aref range 1) (coerce (max from to) 'number))
;; 	r))
;; (defun lbound-of (range)
;;   (aref range 0))
;; (defun ubound-of (range)
;;   (aref range 1))

@export
@export-accessors
(defclass range (diameter-based-mixin)
  ((from :type number :initarg :from :reader range-from)
   (to :type number :initarg :to :reader range-to)))

(defmethod print-object ((v range) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (from to) v
      (format stream "[~A ~A]" from to))))

@export
(defun make-range (from to)
  @type number from to
  (make-instance 'range
                 :from (min from to)
                 :to (max from to)))

@export
(defun make-range-coerce (from to)
  (make-range (desired from)
              (desired to)))

(defmethod diameter ((rng range))
  (with-slots (from to) rng
    (- to from)))

(defmethod center-of ((rng range))
  (with-slots (from to) rng
    (* (+ to from) 0.5d0)))

(defmethod contains-p ((rng range) (n number))
  (with-slots (from to) rng
    (cond ((< from n to) :in)
          ((= from n) :min)
          ((= n to) :max)
          (t nil))))


(defmethod congruent-p ((r1 range) (r2 range))
  (and (=~ (range-from r1) (range-from r2))
       (=~ (range-to r1) (range-to r2))))

(define-permutation-methods congruent-p ((r1 range) (r2 null))
  nil)

(declaim (inline %intersects-p))
(defun %intersects-p (r1 r2)
  @type range r1 r2
  (or (and (< (range-from r1) (range-to r2))
           (<= (range-to r2) (range-to r1)))
      (and (<= (range-from r1) (range-from r2))
           (< (range-from r2) (range-to r1)))))

(defmethod intersects-p ((r1 range) (r2 range))
  (or (%intersects-p r1 r2)
      (%intersects-p r2 r1)))


(define-permutation-methods intersects-p
    ((r1 range) (r2 (eql nil)))
  nil)

(defmethod region-product ((r1 range) (r2 range))
  (if (intersects-p r1 r2)
      (make-range (max (range-from r1)
                       (range-from r2))
                  (min (range-to r1)
                       (range-to r2)))
      nil))

(defmethod region-sum ((r1 range) (r2 range))
  (make-range (min (range-from r1)
                   (range-from r2))
              (max (range-to r1)
                   (range-to r2))))

(define-permutation-methods region-sum
    ((r1 range) (r2 (eql nil)))
  r1)

(define-permutation-methods region-product
    ((r1 range) (r2 (eql nil)))
  nil)
