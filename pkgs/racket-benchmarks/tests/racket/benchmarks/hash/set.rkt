#lang racket/base
(require "config.rkt")

(with-hash-variants

  'add-to-empty
  (times
   (let loop ([ht EMPTY] [i L])
     (if (zero? i)
         (void (unknown ht))
         (loop (hash-set ht KEY (MAKE-VAL 'true))
               (sub1 i)))))

  'add-many
  (times
   (for ([i (in-range Q)])
     (let loop ([ht EMPTY] [i 100])
       (if (zero? i)
           (void (unknown ht))
           (loop (hash-set ht (MAKE-KEY i) (MAKE-VAL 'true))
                 (sub1 i))))))

  'add-same
  (times
   (for ([i (in-range Q)])
     (let loop ([ht EMPTY] [i 100])
       (if (zero? i)
           (void (unknown ht))
           (loop (hash-set ht (MAKE-KEY 'a) (MAKE-VAL 'true))
                 (sub1 i))))))

  (void))
