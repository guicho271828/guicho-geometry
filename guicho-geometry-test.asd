#|
This file is a part of guicho-geometry project.
Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage guicho-geometry-test-asd
  (:use :cl :asdf))
(in-package :guicho-geometry-test-asd)

(defsystem guicho-geometry-test
    :author "Masataro Asai"
    :license "LLGPL"
    :depends-on (:guicho-geometry
                 :optima
                 :fiveam)
    :components ((:module "t"
                          :components
                          ((:file "guicho-geometry"))))
    :perform (load-op :after (op c) (asdf:clear-system c)))
