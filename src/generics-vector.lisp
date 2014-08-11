(in-package :guicho-geometry)

(annot:enable-annot-syntax)

@export
@doc "returns a resized VECTOR-OR-DIRECTIONABLE
 whose length is same as LENGTH"
(defgeneric resize (vector-or-directionable length))

@export
@doc "returns the sum of the vectors"
(defun add (&rest vectors)
  (reduce #'binary-nadd vectors :initial-value (2dv 0 0)))

@export
@doc "returns the sum of the vectors. same as add, but it modifies the vector"
(defun nadd (&rest vectors)
  (if (null vectors)
      (2dv 0 0)
      (reduce #'binary-nadd vectors)))

(defgeneric binary-add (v1 v2))
(defgeneric binary-nadd (v1 v2))

@export
@doc "returns the difference of the vectors"
(defun sub (&rest vectors)
  (cond
    ((null vectors) (2dv 0 0))
    ((null (cdr vectors)) (nsub-vector (2dv 0 0) (car vectors)))
    (t (let ((v (car vectors)))
         (reduce #'binary-nsub (cdr vectors)
                 :initial-value (2dv (x-of v) (y-of v)))))))

@export
@doc "returns the sum of the vectors. same as add, but it modifies the vector"
(defun nsub (&rest vectors)
  (cond
    ((null vectors) (2dv 0 0))
    ((null (cdr vectors))
     (let ((v (car vectors)))
       (with-slots (x y) v
         (setf x (- x) y (- y))
         v)))
    (t (reduce #'binary-nsub vectors))))

(defgeneric binary-sub (v1 v2))
(defgeneric binary-nsub (v1 v2))

@export
@doc "returns the length of a vector"
(defun norm (v)
  @type 2dvector v
  @inline norm2
  (sqrt (norm2 v)))

@export
@doc "returns the square of the length of a vector"
(defgeneric norm2 (v1))

@export
@doc "returns the scalar product of two vectors"
(defgeneric dot (v1 v2))

@export
@doc "returns a vector whose length is 1"
(defgeneric normalize (v1))
