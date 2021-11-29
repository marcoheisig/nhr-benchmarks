(in-package #:nhr-benchmarks)

(defvar *benchmarks* '())

(defun find-benchmark (benchmark-name &optional (errorp t))
  (flet ((fail ()
           (when errorp
             (error "There is no benchmark named ~S." benchmark-name))
           (return-from find-benchmark nil)))
    (etypecase benchmark-name
      (string
       (multiple-value-bind (symbol found)
           (find-symbol (string-upcase benchmark-name) #.*package*)
         (when (not found) (fail))
         (find-benchmark symbol)))
      (symbol
       (let ((found (find benchmark-name *benchmarks* :key #'benchmark-name)))
         (when (not found) (fail))
         found))
      (t (fail)))))

(defgeneric benchmark-name (benchmark))

(defgeneric run-benchmark (benchmark &key timeout memory))

(defgeneric benchmark-memory-requirement (benchmark size))

(defgeneric benchmark-flops (benchmark size repetitions))

(defgeneric benchmark-memory-traffic (benchmark size repetitions))

(defgeneric benchmark-duration-estimate (benchmark size repetitions))

(defgeneric perform-benchmark (benchmark size repetitions))

(defclass benchmark ()
  ((%name
    :type symbol
    :initarg :name
    :initform (alexandria:required-argument :name)
    :reader benchmark-name)))

(defmethod initialize-instance :after ((benchmark benchmark) &key &allow-other-keys)
  (with-accessors ((name benchmark-name)) benchmark
    (setf *benchmarks* (remove name *benchmarks* :key #'benchmark-name))
    (push benchmark *benchmarks*)))

(defmethod run-benchmark ((benchmark benchmark) &key timeout memory)
  ;; Calculate the maximum benchmark size that fits into the specified
  ;; memory requirement.
  (let* ((size
           (binary-search
            0
            (1- array-total-size-limit)
            (lambda (x) (<= (benchmark-memory-requirement benchmark x) memory))))
         (repetitions
           (binary-search
            0
            (expt 10 12)
            (lambda (x) (<= (benchmark-duration-estimate benchmark size x) timeout))))
         (flops (benchmark-flops benchmark size repetitions)))
    (if (zerop flops)
        (format t "~&~,2E Bytes/s~%"
                (/ (benchmark-memory-traffic benchmark size repetitions)
                   (perform-benchmark benchmark size repetitions)))
        (format t "~&~,2E FLOP/s~%"
                (/ flops (perform-benchmark benchmark size repetitions))))))
