(in-package :nhr-benchmarks)

(defclass copy.lisp (benchmark)
  ())

(defun copy.lisp (dst src)
  (declare (type (simple-array f64) dst src))
  (do-vectorized (index 0 (array-total-size src))
    (:unroll 4)
    (setf (f64-row-major-aref dst index)
          (f64-row-major-aref src index))))

(defmethod benchmark-memory-requirement
    ((copy.lisp copy.lisp) size)
  (* 2 size 8))

(defmethod benchmark-flops
    ((copy.lisp copy.lisp) size repetitions)
  0)

(defmethod benchmark-memory-traffic
    ((copy.lisp copy.lisp) size repetitions)
  (* repetitions 2 size 8))

(defmethod benchmark-duration-estimate
    ((copy.lisp copy.lisp) size repetitions)
  (/ (benchmark-memory-traffic copy.lisp size repetitions)
     1d10))

(defmethod perform-benchmark
    ((copy.lisp copy.lisp) size repetitions)
  (let ((a (make-array size :element-type 'double-float :initial-element 0d0))
        (b (make-array size :element-type 'double-float :initial-element 0d0)))
    (let ((t0 (timestamp)))
      (loop repeat repetitions do
        (copy.lisp a b))
      (- (timestamp) t0))))

(make-instance 'copy.lisp :name 'copy.lisp)
