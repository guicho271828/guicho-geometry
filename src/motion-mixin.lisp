(in-package :lmates.geometry)
(annot:enable-annot-syntax)

(optimize*)

@export @export-accessors @doc "This is a mixin which stores the
motion of an object. Slot TIME, inherited from `time-mixin',
represents the current absolute time of the object. Slot MOTION, an
instance of `2+1dvector', represents planned absolute center point of
an object in the future. TIME slot in MOTION is also an absolute
time."
(defclass motion-mixin (time-mixin)
  ((motion :type 2+1dvector 
		   :initarg :motion
		   :initform (2+1dv 0.0d0 0.0d0 0.0d0)
		   :accessor motion)
   (dt :type *desired-type*)
   (velocity :type 2dvector)))

;; (defmethod motion ((m motion-mixin))
;;   (with-memoising-slot (motion m)
;; 	(with-slots (time) m
;; 	  (with-accessors ((c center-of)) m
;; 		(2+1dv (x-of c) (y-of c) (d+ +dt+ time))))))

(defmethod (setf motion) :after ((v 2+1dvector) (m motion-mixin))
  (slot-makunbound m 'velocity)
  (slot-makunbound m 'dt))

@export
(defun dt (movable)
  (with-memoising-slot (dt movable)
	(with-slots (motion time) movable
	  (d- (t-of motion) time))))

@export
(defgeneric motion-velocity-of (movable))
@export
(defgeneric (setf motion-velocity-of) (vector movable))
(defmethod (setf motion-velocity-of) ((v 2dvector) (m motion-mixin))
  (with-slots (motion) m
	(let ((vt (nadd-vector (scale-vector v (dt m))
						   (center-of m))))
	  (setf (x-of motion) (x-of vt)
			(y-of motion) (y-of vt)))))

(defmethod motion-velocity-of ((movable motion-mixin))
  (with-memoising-slot (velocity movable)
	(with-slots (motion) movable
	  (nscale-vector (sub motion (center-of movable))
					 (d/ (dt movable))))))

(defmethod motion-velocity-of ((shape 2dshape))
  (2dv 0.0d0 0.0d0))

;; 同様に、ボトルネックはどんどんメモ化していけばいい

@export
(defun time-range (movable)
  (make-range (t-of movable)
			  (t-of (motion movable))))

@export
(defgeneric sharing-time-range (shape1 shape2))
(defmethod sharing-time-range ((m1 motion-mixin) (m2 motion-mixin))
  (region-product (time-range m1)
				  (time-range m2)))

(define-permutation-methods sharing-time-range ((m1 motion-mixin) shape2)
  (time-range m1))


@export
(defgeneric future-center-position (movable time))
(defmethod future-center-position ((m motion-mixin) time)
  (nadd-vector (scale-vector (motion-velocity-of m)
							 (d- time (t-of m)))
			   (center-of m)))
(defmethod future-center-position ((s 2dshape) time)
  (shallow-copy (center-of s)))

@export
(defgeneric future-shape (movable time))
(defmethod future-shape ((m motion-mixin) time)
  (translate m (scale-vector
				(motion-velocity-of m)
				(d- time (t-of m)))))

(defmethod future-shape ((s 2dshape) time) s)

@export
@doc "circular boundaryが接触する時間の範囲, for no range returns nil"
(defun time-range-of-circular-intersection (movable1 movable2)
  (when-let ((t-range (sharing-time-range movable1 movable2)))
	(let* ((time (range-from t-range))
		   (dp (sub-vector (future-center-position movable1 time)
						   (future-center-position movable2 time)))
		   (dv (sub-vector (motion-velocity-of movable2)
						   (motion-velocity-of movable1)))
		   (dv1 (normalize dv))
		   (perp (dot dp (rotate90 dv1)))
		   (r (d+ (radius movable1) (radius movable2))))
	  (if (d< (dabs r) (dabs perp))
		  nil
		  (let* ((dvnorm (norm dv))
				 (t-center  (d+ time (d/ (dot dp dv1) dvnorm)))
				 (t-width/2 (d/ (dsqrt (d- (d* r r) (d* perp perp)))
								dvnorm)))
			(values
			 (region-product
			  t-range
			  (make-range
			   (d- t-center t-width/2)
			   (d+ t-center t-width/2)))
			 (make-range
			   (d- t-center t-width/2)
			   (d+ t-center t-width/2))))))))

@export
(defun intersect-at-time (m1 m2 time)
  (intersects-p
   (future-shape m1 time)
   (future-shape m2 time)))

(defun %when-intersect-last (m1 m2 dl candidate)
  (iter
	(while dl)
	(for time = (dlist:dlist-pop dl))
	(when (intersect-at-time m1 m2 time)
	  (return time))
	(finally
	 (return candidate))))

(defparameter +minumum-dt+ 1.0d-3)

(defun %when-intersect (m1 m2 dl dt candidate)
  (if (d< dt +minumum-dt+)
	  (%when-intersect-last m1 m2 dl candidate)
	  (iter
		(with next-dl = nil)
		(with next-dt = (d* 0.5d0 dt))
		(while dl)
		(for time = (dlist:dlist-pop dl))
		;(print time)
		(when (intersect-at-time m1 m2 time)
		  ;; (print :go-back)
		  (return
			(%when-intersect
			 m1 m2 (dlist:dlist (d- time next-dt)) next-dt time)))
		(dlist:dlist-push (d- time next-dt) next-dl :at-end t)
		(dlist:dlist-push (d+ time next-dt) next-dl :at-end t)
		(finally
		 ;; (print :go-further)
		 (return
		   (%when-intersect m1 m2 next-dl next-dt candidate))))))

@export
@doc "Returns the remaining *time span* that those two
`movable-mixin's will collide. if no collision was detected, return
nil."
(defun when-intersect (m1 m2)
  (when-let ((time-range (time-range-of-circular-intersection m1 m2)))
	(with-accessors ((from range-from) (to range-to)) time-range
	  (cond
		((intersect-at-time m1 m2 from)
		 ;; (print :detected-at-the-beginning)
		 from)
		((intersect-at-time m1 m2 to)
		 ;; (print :detected-at-the-end)
		 (%when-intersect
		  m1 m2
		  (dlist:dlist (d* (d+ from (d* 3.0d0 to)) 0.25d0))
		  (radius time-range)
		  to))
		(t
		 ;; (print :no-collision-at-ends)
		 (%when-intersect
		  m1 m2
		  (dlist:dlist (center-of time-range))
		  (diameter time-range)
		  nil))))))

;; ;; ex. n=4, 2^4=16
;; ;; 16 :first
;; ;; 16でぶつかってる>手前に

;; 0  ; 0でぶつかってる -> そこが解
;; 8  ; 8

;; ;; 8で
;; ;; ぶつかってる   -> 手前に移動 4へ (-4)
;; ;; ぶつかってない -> 後ろと手前両方調べる 4と12へ 先に4

;; 4  ; 4 ;; ぶつかってる --> 2へ移動 ぶつかってない-> 2,6を追加して次のキューへ
;; 12 ; 4+8 ;; ぶつかってる --> 10へ移動 ぶつかってない -> 10,14を追加して次のキューへ

;; 2  ; 2
;; 6  ; 2+4
;; 10 ; 2+4+4
;; 14 ; 2+4+4+4
