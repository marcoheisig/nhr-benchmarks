(in-package :nhr-benchmarks)

(include "jacobi.c")
(flag "-march=native -Ofast")
(flag #.(concatenate
         'string
         "-I"
         (namestring
          (asdf:component-pathname
           (asdf:find-component "nhr-benchmarks" '("benchmarks"))))))
(defwrapper ("jacobi" jacobi.c) :void
  (dst (:pointer :double))
  (src (:pointer :double))
  (rows :unsigned-int)
  (columns :unsigned-int))
