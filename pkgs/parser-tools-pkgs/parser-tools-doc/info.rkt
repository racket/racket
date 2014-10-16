#lang info

(define collection 'multi)
(define deps '("base"))
(define build-deps '("scheme-lib"
                     "racket-doc"
                     "syntax-color-doc"
                     "parser-tools-lib"
                     "scribble-lib"))
(define update-implies '("parser-tools-lib"))

(define pkg-desc "documentation part of \"parser-tools\"")

(define pkg-authors '(mflatt))
