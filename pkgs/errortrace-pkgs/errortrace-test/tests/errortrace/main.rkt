#lang racket/base

(require tests/eli-tester 
         "wrap.rkt" 
         "alert.rkt"
         "phase-1.rkt"
         "phase-1-eval.rkt"
         "begin.rkt")

(module test racket/base
  (displayln "run as program for tests"))

(wrap-tests)

(test do (alert-tests))

(phase-1-tests)
(phase-1-eval-tests)
(begin-for-syntax-tests)
