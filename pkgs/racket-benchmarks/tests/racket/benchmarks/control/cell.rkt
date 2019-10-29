#lang racket/base

(require racket/include)

(include "config.rktl")

'----------------------------------------

(define c (make-thread-cell #f))

'cell-ref
(times
 (let loop ([i L] [a #f])
   (if (zero? i)
       a
       (loop (sub1 i) (thread-cell-ref c)))))

'cell-set!
(times
 (let loop ([i L])
   (if (zero? i)
       (thread-cell-ref c)
       (begin
         (thread-cell-set! c i)
         (loop (sub1 i))))))

'cell-ref-after-set
(times
 (let loop ([i L] [a #f])
   (if (zero? i)
       a
       (loop (sub1 i) (thread-cell-ref c)))))
