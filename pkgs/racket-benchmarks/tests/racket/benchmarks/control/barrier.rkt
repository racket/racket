#lang racket/base
(require racket/include)

(include "config.rktl")

;; ----------------------------------------

'barrier
(times
 (let ([one (lambda () 1)])
   (let loop ([i M])
     (unless (zero? i)
       (loop (- i (call-with-continuation-barrier one)))))))

'thunk+barrier
(times
 (let loop ([i M])
   (unless (zero? i)
     (loop (call-with-continuation-barrier (lambda () (sub1 i)))))))
