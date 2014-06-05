

(in-package :lmates.geometry)
(annot:enable-annot-syntax)

@export
(defclass radius-based-mixin ()())

(defmethod diameter ((o radius-based-mixin))
  (d* 2.0d0 (radius o)))

@export
(defclass diameter-based-mixin ()())

(defmethod radius ((o diameter-based-mixin))
  (d* 2.0d0 (diameter o)))