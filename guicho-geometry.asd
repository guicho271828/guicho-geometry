#|
  This file is a part of guicho-geometry project.
  Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  my personal geometry classes library

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage guicho-geometry-asd
  (:use :cl :asdf))
(in-package :guicho-geometry-asd)

(defsystem guicho-geometry
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:iterate
               :alexandria
               :cl-annot
               :anaphora
			   :guicho-utilities)
  :components ((:module "src"
                :components
                ((:file :package)
				 (:file :generics)
				 (:file :generics-vector)
				 ;; basic classes
				 (:file :range)
				 (:file :2dvector)
				 (:file :2+1dvector)
				 (:file :2dshape)
				 (:file :2dmatrix)
				 ;;(:file :3dvector) (:file :3dshape) (:file :3dmatrix)
				 ;; mixins
				 (:file :infinite-shape)
				 (:file :directionable)
				 (:file :2dpolygon)
				 (:file :radius-diameter)
				 (:file :motion-mixin)
				 ;; classes
				 (:file :2dsegment) (:file :2drectangle)
				 (:file :2dline) (:file :2dcircle)
				 (:file :2dobb)
				 (:file :movable-2dobb)
				 ;; (:file :3dsegment) (:file :hexahedron)
				 ;; (:file :3dplane) (:file :3dline)
				 ;; (:file :3dsphere) ;; (:file :3+1dvector)	 
				 (:file :intersects-p))))
  :description "my personal geometry classes library"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op guicho-geometry-test))))
