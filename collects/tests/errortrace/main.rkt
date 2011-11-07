#lang racket/base

(require tests/eli-tester 
         "wrap.rkt" 
         "alert.rkt"
         "phase-1.rkt")

(wrap-tests)

(test do (alert-tests))

(phase-1-tests)
