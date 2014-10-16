#lang info

(define collection 'multi)

(define deps '("base"
               "compiler-lib"
               "rackunit-lib"
               "redex-lib"
               "redex-examples"
               "math-lib"
               "plot-lib"))

(define build-deps '())

(define pkg-desc "PLT Redex Benchmark")

(define pkg-authors '(robby bfetscher))
