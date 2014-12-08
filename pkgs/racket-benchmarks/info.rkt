#lang info

(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "r5rs-lib"
               "scheme-lib"
               "srfi-lite-lib"
               "racket-test"
               "typed-racket-lib"))

(define pkg-desc "Racket benchmarks")
(define pkg-authors '(eli jay mflatt robby samth stamourv))
