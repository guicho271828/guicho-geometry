
(declaim (optimize (debug 3)))
(in-package :cl-user)
(defpackage guicho-geometry
  (:use :cl
        :annot.class
        :annot.eval-when
        :annot.doc
        :annot.slot
        :anaphora
        :iterate
        :alexandria
        :guicho-utilities)
  (:shadow :rotate))

(in-package :guicho-geometry)
(annot:enable-annot-syntax)

