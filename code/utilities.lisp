(in-package #:nhr-benchmarks)

(defun timestamp ()
  (/ (coerce (get-internal-real-time) 'double-float)
     (coerce internal-time-units-per-second 'double-float)))

;;; Find the highest integer in the interval between START (inclusive) and
;;; END (exclusive) that satisfies TEST.
(defun binary-search (start end test)
  (declare (alexandria:non-negative-fixnum start end)
           (function test))
  (unless (< start end)
    (error "The interval start ~D is not less than its end ~D."
           start end))
  (if (= start (1- end))
      start
      (let ((mid (floor (+ start end) 2)))
        (if (funcall test mid)
            (binary-search mid end test)
            (binary-search start mid test)))))
