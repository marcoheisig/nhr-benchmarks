LISP := ${HOME}/local/bin/sbcl
FILES := $(wildcard code/*.lisp)

nhr-benchmarks: ${FILES}
	${LISP} --eval "(require \"asdf\")" \
	        --eval "(asdf:make :$@)" \
	        --eval "(uiop:quit)"
	${LISP} --eval "(require \"asdf\")" \
	        --eval "(uiop:run-program \
	                  (list \"cp\" \
	                    (namestring (asdf:output-file 'asdf:program-op :$@)) \
	                    (namestring #p\"./\")))" \
	        --eval "(uiop:quit)"
