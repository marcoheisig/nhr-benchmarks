(in-package :nhr-benchmarks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; jacobi.lisp

(defclass jacobi.lisp (benchmark)
  ((%rows
    :initarg :rows
    :initform 4
    :reader rows)))

(defun jacobi.lisp (dst src)
  (declare (type (simple-array f64 2) dst src))
  (loop for i from 1 below (1- (array-dimension dst 0)) do
    (do-vectorized (j 1 (1- (array-dimension dst 1)))
      (:unroll 2)
      (setf (f64-aref dst i j)
            (f64* 0.25d0
                  (f64+
                   (f64-aref src i (1+ j))
                   (f64-aref src i (1- j))
                   (f64-aref src (1+ i) j)
                   (f64-aref src (1- i) j)))))))

(defmethod benchmark-memory-requirement
    ((jacobi.lisp jacobi.lisp) size)
  (* 2 (rows jacobi.lisp) size 8))

(defmethod benchmark-flops
    ((jacobi.lisp jacobi.lisp) size repetitions)
  (* repetitions
     (- (rows jacobi.lisp) 2)
     (- size 2)
     4))

(defmethod benchmark-memory-traffic
    ((jacobi.lisp jacobi.lisp) size repetitions)
  (* repetitions
     2
     (- (rows jacobi.lisp) 2)
     (- size 2)
     8))

(defmethod benchmark-duration-estimate
    ((jacobi.lisp jacobi.lisp) size repetitions)
  (/ (benchmark-flops jacobi.lisp size repetitions) 3e9))

(defmethod perform-benchmark
    ((jacobi.lisp jacobi.lisp) size repetitions)
  (let ((rows 4)
        (cols size))
    (let ((a (make-array (list rows cols) :element-type 'double-float :initial-element 0d0))
          (b (make-array (list rows cols) :element-type 'double-float :initial-element 0d0)))
      (let ((t0 (timestamp)))
        (loop repeat (ceiling repetitions 2) do
          (jacobi.lisp a b)
          (jacobi.lisp b a))
        (- (timestamp) t0)))))

(make-instance 'jacobi.lisp :name 'jacobi.lisp)
