#lang racket/base
(require racket/include)

(include "config.rktl")

(define (f x) x)
(set! f f)

'----------------------------------------

'hasheq-add-remove
(times
 (let loop ([ht #hasheq()] [i L])
   (if (zero? i)
       (void (f ht))
       (loop (hash-remove (hash-set ht 'a #t) 'a)
             (sub1 i)))))

'hasheq-adds
(times
 (for ([i (in-range Q)])
   (let loop ([ht #hasheq()] [i 100])
     (if (zero? i)
         (void (f ht))
         (loop (hash-set ht i 'true)
               (sub1 i))))))

'hasheq-adds/#t
(times
 (for ([i (in-range Q)])
   (let loop ([ht #hasheq()] [i 100])
     (if (zero? i)
         (void (f ht))
         (loop (hash-set ht i #t)
               (sub1 i))))))

'hasheq-addsame
(times
 (for ([i (in-range Q)])
   (let loop ([ht #hasheq()] [i 100])
     (if (zero? i)
         (void (f ht))
         (loop (hash-set ht 'a 'true)
               (sub1 i))))))

'hasheq-removes
(times
 (let ([ht (for/hasheq ([i (in-range 100)])
             (values i i))])
   (for ([i (in-range Q)])
     (let loop ([ht ht] [i 100])
       (if (zero? i)
           (void (f ht))
           (loop (hash-remove ht i)
                 (sub1 i)))))))

'hasheq-ref
(times
 (let ([ht (for/hasheq ([i (in-range 100)])
             (values i i))])
   (for ([i (in-range Q)])
     (let loop ([v #f] [i 100])
       (if (zero? i)
           (void (f v))
           (loop (hash-ref ht i #f)
                 (sub1 i)))))))

'hasheq-reffail
(times
 (let ([ht (for/hasheq ([i (in-range 100)])
             (values i i))])
   (for ([i (in-range Q)])
     (let loop ([v #f] [i 100])
       (if (zero? i)
           (void (f v))
           (loop (hash-ref ht 'not-there #f)
                 (sub1 i)))))))
