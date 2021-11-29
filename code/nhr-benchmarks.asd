(defsystem :nhr-benchmarks
  :description "Benchmarks for the NHR@FAU."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :defsystem-depends-on
  ("cffi-grovel")

  :depends-on
  ("net.didierverna.clon"
   "closer-mop"
   "cffi"
   "cl-ppcre"
   "sb-simd"
   "alexandria"
   "trivia")

  :build-pathname "nhr-benchmarks"
  :build-operation "program-op"
  :entry-point "nhr-benchmarks::main"

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "clon")
   (:file "benchmark")
   (:file "nhr-benchmarks")
   (:module "benchmarks"
    :components
    ((:file "jacobi")
     (:static-file "jacobi.c")
     (:cffi-wrapper-file "c-wrappers")
     (:file "c-benchmarks")))))
