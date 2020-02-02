#lang racket/base
(require racket/include)

(include "config.rktl")

(define (f x) x)
(set! f f)

'----------------------------------------

;; Capturing continuations
'capture
(times
 (let loop ([i M] [k #f])
   (if (zero? i)
       k
       (loop (sub1 i) (call/cc (lambda (k) k))))))

;; Applying a continuation
'apply
(times
 (let ([loop #f])
   (let ([i (call/cc
             (lambda (k)
               (set! loop k)
               M))])
     (if (zero? i)
         0
         (loop (sub1 i))))))


