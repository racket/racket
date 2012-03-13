#lang racket/base

(require tests/eli-tester 
         "wrap.rkt" 
         "alert.rkt"
         "phase-1.rkt"
         "phase-1-eval.rkt"
         "begin.rkt")

(wrap-tests)

(test do (alert-tests))

(phase-1-tests)
(phase-1-eval-tests)
(begin-for-syntax-tests)
