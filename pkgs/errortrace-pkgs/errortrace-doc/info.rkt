#lang info
(define collection 'multi)
(define build-deps '("racket-doc"
                     "base"
                     "errortrace-lib"
                     "scribble-lib"))
(define update-implies '("errortrace-lib"))

(define pkg-desc "documentation part of \"errortrace\"")

(define pkg-authors '(mflatt))
