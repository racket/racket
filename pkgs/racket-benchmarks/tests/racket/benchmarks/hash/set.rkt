#lang racket/base
(require "config.rkt"
         racket/list)

(define shuffled
  (parameterize ([current-pseudo-random-generator
                  (make-pseudo-random-generator)])
    (random-seed 12745)
    (shuffle (for/list ([i (in-range K)])
               i))))

(with-hash-variants

  'set-in-empty
  (times
   (let loop ([ht EMPTY] [i L])
     (if (zero? i)
         (void (unknown ht))
         (loop (hash-set ht KEY (MAKE-VAL 'true))
               (sub1 i)))))

  'set-many-in-order
  (times
   (for ([i (in-range Q)])
     (let loop ([ht EMPTY] [i K])
       (if (zero? i)
           (void (unknown ht))
           (loop (hash-set ht (MAKE-KEY i) (MAKE-VAL 'true))
                 (sub1 i))))))

  'set-many
  (times
   (for ([i (in-range Q)])
     (let loop ([ht EMPTY] [l shuffled])
       (if (null? l)
           (void (unknown ht))
           (loop (hash-set ht (car l) (MAKE-VAL 'true))
                 (cdr l))))))

  'set-same
  (times
   (for ([i (in-range Q)])
     (let loop ([ht EMPTY] [i K])
       (if (zero? i)
           (void (unknown ht))
           (loop (hash-set ht (MAKE-KEY 'a) (MAKE-VAL 'true))
                 (sub1 i))))))

  (void))
