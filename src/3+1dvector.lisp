

(in-package :guicho-geometry)
(annot:enable-annot-syntax)


@export
(defclass 3+1dvector (time-mixin 3dvector)
  ())

@export
(defun 3+1dv (x y z time)
  @type number x
  @type number y
  @type number z
  @type number time
  (make-instance '3+1dvector :x x :y y :z z :t time))

(defmethod without-z ((v 3+1dvector))
  (with-slots (x y z time) v
    (2+1dv x y time)))
(defmethod nwithout-z ((v 3+1dvector))
  (change-class v '2+1dvector))

(defmethod without-t ((v 3+1dvector))
  (with-slots (x y z) v
    (3dv x y z)))
(defmethod nwithout-t ((v 3+1dvector))
  (change-class v '3dvector))


@export
@export-accessors
(defclass 3+1dsegment (time-directionable-mixin 3dsegment)
  ((from :type 3+1dvector 
         :initarg :current
         :initarg :current-pos
         :accessor current-pos)
   (to :type 3+1dvector 
       :initarg :future
       :initarg :future-pos
       :accessor future-pos)))

(defmethod without-z ((s 3+1dsegment))
  (make-instance '2+1dsegment
                 :from (without-z (current-pos s))
                 :to (without-z (future-pos s))))


(defmethod print-object ((v 3+1dsegment) stream)
  (print-unreadable-object (v stream :type t)
    (with-slots (from to) v
      (format stream "[~a,~a,~a,~a] [~a,~a,~a,~a]"
              (x-of from) (y-of from) (z-of from) (t-of from)
              (x-of to) (y-of to) (z-of to) (t-of to)))))
