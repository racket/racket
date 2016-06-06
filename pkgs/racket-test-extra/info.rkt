#lang info

(define collection 'multi)

(define pkg-desc "Additional Racket test suites")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
(define build-deps '("base"
                     "redex-lib"
                     "scheme-lib"
                     "rackunit-lib"
                     "serialize-cstruct-lib"))
