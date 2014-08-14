#lang info

(define collection 'multi)

(define build-deps '("scribble-lib"
                     "html-lib"
                     "racket-doc"))
(define deps '("base"))
(define update-implies '("html-lib"))

(define pkg-desc "documentation part of \"html\"")

(define pkg-authors '(jay mflatt))
