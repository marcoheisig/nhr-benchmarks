(in-package #:net.didierverna.clon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Duration

(defoption duration ()
  ((argument-name :initform "DURATION"))
  (:documentation "The DURATION class.
This class implements options whose values describe a duration in seconds."))

(define-condition invalid-duration (invalid-value)
  ()
  (:default-initargs
   :comment "Valid values are of the form ([0-9]*[.])?[0-9]+[smh].")
  (:report
   (lambda (error stream)
     (format stream "Option ~A: invalid duration ~S.~@[~%~A~]"
             (long-name (option error))
             (value error)
             (comment error)))))

(defmethod check ((option duration) (value t))
  (if (typep value 'double-float)
      value
      (error 'invalid-duration
             :option option
             :value value)))

(defmethod convert ((option duration) (argument string))
  "Convert a duration argument string to a double-precision floating point
that denotes the number of seconds."
  (multiple-value-bind (whole substrings)
      (cl-ppcre:scan-to-strings "([0-9]+\\.?[0-9]*)([smh])" argument)
    (unless (and whole
                 (every #'stringp substrings)
                 (= (length whole) (length argument)))
      (error 'invalid-duration
             :option option
             :value argument))
    (let ((float (read-from-string (aref substrings 0)))
          (scale (ecase (schar (aref substrings 1) 0)
                   (#\s 1d0)
                   (#\m 60d0)
                   (#\h (* 60d0 60d0)))))
      (* float scale))))

(defmethod stringify ((option duration) value)
  (with-output-to-string (stream)
    (cond ((<= 0d0 value 60d0)
           (format stream "~,2Fs" value))
          ((<= 60d0 value 3600d0)
           (format stream "~,2Fm" (/ value 60d0)))
          (t
           (format stream "~,2Fh" (/ value 3600d0))))))

(defmethod print-object ((option duration) stream)
  (format stream "~S" (long-name option)))

(defun make-duration (&rest keys
                      &key
                        short-name long-name description
                        argument-name argument-type
                        env-var fallback-value default-value hidden)
  (declare (ignore short-name long-name description
                   argument-name argument-type
                   env-var fallback-value default-value hidden))
  (apply #'make-instance 'duration keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bytes

(defoption bytes ()
  ((argument-name :initform "BYTES"))
  (:documentation "The BYTES class.
This class implements options whose values describe a number of bytes."))

(define-condition invalid-bytes (invalid-value)
  ()
  (:default-initargs
   :comment "Valid byte specifications are of the form [0-9]+([kMGTPEZY]?[bB])?.")
  (:report
   (lambda (error stream)
     (format stream "Option ~A: invalid byte specification ~S.~@[~%~A~]"
             (long-name (option error))
             (value error)
             (comment error)))))

(defmethod check ((option bytes) (value t))
  (if (typep value 'unsigned-byte)
      value
      (error 'invalid-bytes
             :option option
             :value value)))

(defmethod convert ((option bytes) (argument string))
  "Convert a bytes argument string to an integer that denotes the number of
bytes."
  (multiple-value-bind (whole substrings)
      (cl-ppcre:scan-to-strings "([0-9]+)([kMGTPEZY]?[bB])?" argument)
    (unless (and whole
                 (stringp (aref substrings 0))
                 (= (length whole) (length argument)))
      (error 'invalid-bytes
             :option option
             :value argument))
    (* (read-from-string (aref substrings 0))
       (if (null (aref substrings 1))
           1
           (ecase (schar (aref substrings 1) 0)
             (#\k (expt 1000 1))
             (#\M (expt 1000 2))
             (#\G (expt 1000 3))
             (#\T (expt 1000 4))
             (#\P (expt 1000 5))
             (#\E (expt 1000 6))
             (#\Z (expt 1000 7))
             (#\Y (expt 1000 8)))))))

(defmethod stringify ((option bytes) value)
  (with-output-to-string (stream)
    (cond ((> value (expt 1000 8))
           (format stream "~,2FYB" (/ value (expt 1000 8))))
          ((> value (expt 1000 7))
           (format stream "~,2FZB" (/ value (expt 1000 7))))
          ((> value (expt 1000 6))
           (format stream "~,2FEB" (/ value (expt 1000 6))))
          ((> value (expt 1000 5))
           (format stream "~,2FPB" (/ value (expt 1000 5))))
          ((> value (expt 1000 4))
           (format stream "~,2FTB" (/ value (expt 1000 4))))
          ((> value (expt 1000 3))
           (format stream "~,2FGB" (/ value (expt 1000 3))))
          ((> value (expt 1000 2))
           (format stream "~,2FMB" (/ value (expt 1000 2))))
          ((> value (expt 1000 1))
           (format stream "~,2FkB" (/ value (expt 1000 1))))
          (t
           (format stream "~,2FB" value)))))

(defmethod print-object ((option bytes) stream)
  (format stream "~S" (long-name option)))

(defun make-bytes (&rest keys
                   &key
                     short-name long-name description
                     argument-name argument-type
                     env-var fallback-value default-value hidden)
  (declare (ignore short-name long-name description
                   argument-name argument-type
                   env-var fallback-value default-value hidden))
  (apply #'make-instance 'bytes keys))
