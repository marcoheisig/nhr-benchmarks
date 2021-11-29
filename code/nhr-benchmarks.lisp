(in-package #:nhr-benchmarks)

(clon:defsynopsis (:postfix "BENCHMARKS...")
  (text :contents "Marco Heisig's NHR@FAU benchmark suite.")
  (group (:header "Options:")
         (bytes
          :short-name "m"
          :long-name "memory"
          :default-value 32000
          :description
          "The number of bytes of memory that should be used for the benchmark.")
         (duration
          :short-name "t"
          :long-name "timeout"
          :default-value 10d0
          :description
          "How long the benchmark should run.")
         (flag
          :short-name "h"
          :long-name "help"
          :description
          "Print this help and exit.")
         (flag
          :short-name "l"
          :long-name "list-benchmarks"
          :description
          "List all available benchmarks and exit.")
         (flag
          :short-name "v"
          :long-name "verbose")))

(defvar *verbose* nil)

(defun main ()
  (clon:make-context)
  (let ((*verbose* (clon:getopt :long-name "verbose"))
        (memory (clon:getopt :long-name "memory"))
        (timeout (clon:getopt :long-name "timeout"))
        (benchmarks (mapcar #'find-benchmark (clon:remainder))))
    (when (clon:getopt :long-name "list-benchmarks")
      (dolist (benchmark *benchmarks*)
        (format t "~&~A~%" (benchmark-name benchmark)))
      (uiop:quit))
    (when (or (clon:getopt :long-name "help")
              (null benchmarks))
      (clon:help)
      (uiop:quit))
    (loop for benchmark in benchmarks do
      (run-benchmark benchmark :memory memory :timeout timeout)))
  (uiop:quit))
