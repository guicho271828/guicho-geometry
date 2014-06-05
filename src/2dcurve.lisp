

(in-package :guicho-geometry)
(annot:enable-annot-syntax)

(defclass 2dcurve-1p (2dshape)
  ((fn :type (function (number) 2dvector)
       :initarg :fn
       :accessor fn-of)))

(defgeneric parametric-at (curve &rest params))

(defmethod parametric-at ((c 2dcurve-1p) &rest params)
  (apply (fn-of c) params))

