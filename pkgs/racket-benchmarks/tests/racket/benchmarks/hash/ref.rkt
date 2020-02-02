#lang racket/base
(require "config.rkt")

(with-hash-variants

  'ref
  (times
   (let ([ht (FOR/HASH ([i (in-range 100)])
               (values (MAKE-KEY i) (MAKE-VAL i)))])
     (for ([i (in-range Q)])
       (let loop ([v #f] [i 100])
         (if (zero? i)
             (void (unknown v))
             (loop (hash-ref ht (MAKE-KEY i) #f)
                   (sub1 i)))))))

  #:only equal
  'ref-large
  (times
   (let ([ht (for/hash ([i (in-range 100)])
               (values (make-large-equal-key/share1 i) i))])
     (for ([i (in-range (quotient Q 10))])
       (let loop ([v #f] [i 100])
         (if (zero? i)
             (void (unknown v))
             (loop (hash-ref ht (make-large-equal-key/share2 i) #f)
                   (sub1 i)))))))

  'ref-fail
  (times
   (let ([ht (FOR/HASH ([i (in-range 100)])
               (values (MAKE-KEY i) (MAKE-VAL i)))])
     (for ([i (in-range Q)])
       (let loop ([v #f] [i 100])
         (if (zero? i)
             (void (unknown v))
             (loop (hash-ref ht OTHER-KEY #f)
                   (sub1 i)))))))

  #:only equal
  'ref-large-fail
  (times
   (let ([ht (for/hash ([i (in-range 100)])
               (values (make-large-equal-key/share1 i) i))]
         [not-there (make-large-equal-key/share2 -1)])
     (for ([i (in-range (quotient Q 10))])
       (let loop ([v #f] [i 100])
         (if (zero? i)
             (void (unknown v))
             (loop (hash-ref ht not-there #f)
                   (sub1 i)))))))

  (void))
