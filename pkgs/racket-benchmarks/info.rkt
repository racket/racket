#lang info

(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "r5rs-lib"
               "scheme-lib"
               "racket-test"
               "typed-racket-lib"
               "plot"
               "draw-lib"
               "gui-lib"
               "pict-lib"))

(define pkg-desc "Racket benchmarks")
(define pkg-authors '(eli jay mflatt robby samth stamourv))

(define license
  '(Apache-2.0 OR MIT))
