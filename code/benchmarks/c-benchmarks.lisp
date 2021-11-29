(in-package :nhr-benchmarks)

(defclass c-benchmark (benchmark)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; jacobi.c

(defclass jacobi.c (c-benchmark)
  ((%rows
    :initarg :rows
    :initform 4
    :reader rows)))

(defmethod benchmark-memory-requirement
    ((jacobi.c jacobi.c) size)
  (* 2 (rows jacobi.c) size 8))

(defmethod benchmark-flops
    ((jacobi.c jacobi.c) size repetitions)
  (* repetitions
     (- (rows jacobi.c) 2)
     (- size 2)
     4))

(defmethod benchmark-memory-traffic
    ((jacobi.c jacobi.c) size repetitions)
  (* repetitions
     2
     (- (rows jacobi.c) 2)
     (- size 2)
     8))

(defmethod benchmark-duration-estimate
    ((jacobi.c jacobi.c) size repetitions)
  (/ (benchmark-flops jacobi.c size repetitions) 3e9))

(defmethod perform-benchmark
    ((jacobi.c jacobi.c) size repetitions)
  (let ((rows 4)
        (cols size))
    (cffi:with-foreign-pointer (a (* rows cols 8))
      (cffi:with-foreign-pointer (b (* rows cols 8))
        (loop for index below (* rows cols) do
          (setf (cffi:mem-ref a :double index) 0d0)
          (setf (cffi:mem-ref b :double index) 0d0))
        (let ((t0 (timestamp)))
          (loop repeat (ceiling repetitions 2) do
            (jacobi.c a b rows cols)
            (jacobi.c b a rows cols))
          (- (timestamp) t0))))))

(make-instance 'jacobi.c :name 'jacobi.c)
