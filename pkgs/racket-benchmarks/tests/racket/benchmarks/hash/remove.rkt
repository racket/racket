#lang racket/base
(require "config.rkt")

(with-hash-variants

  'removes
  (times
   (let ([ht (FOR/HASH ([i (in-range 100)])
               (values (MAKE-KEY i) (MAKE-VAL i)))])
     (for ([i (in-range Q)])
       (let loop ([ht ht] [i 100])
         (if (zero? i)
             (void (unknown ht))
             (loop (hash-remove ht (MAKE-KEY i))
                   (sub1 i)))))))

  'add+remove
  (times
   (let loop ([ht EMPTY] [i L])
     (if (zero? i)
         (void (unknown ht))
         (loop (hash-remove (hash-set ht KEY (MAKE-VAL i)) KEY)
               (sub1 i)))))

  (void))

