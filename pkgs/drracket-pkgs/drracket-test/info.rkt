#lang info

(define collection 'multi)

(define deps '("base"
               "htdp"
               "drracket"))
(define build-deps '("racket-index"
                     "scheme-lib"
                     "at-exp-lib"
                     "rackunit-lib"
                     "compatibility-lib"
                     "gui-lib"
                     "htdp"
                     "compiler-lib"
                     "cext-lib"
                     "string-constants-lib"))
(define update-implies '("drracket"))

(define pkg-desc "tests for \"drracket\"")

(define pkg-authors '(robby))
