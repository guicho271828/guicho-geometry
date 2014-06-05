
(declaim (optimize (debug 3)))
(in-package :cl-user)
(defpackage lmates.geometry
  (:use :cl
		:annot.class
		:annot.eval-when
		:annot.doc
		:annot.slot
		:anaphora
		:iterate
		:alexandria
		:lmates.utilities)
  (:shadow :rotate))

(in-package :lmates.geometry)
(annot:enable-annot-syntax)

