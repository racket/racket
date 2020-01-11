#lang racket/base
(require "config.rkt")

'iterate-keys:eq
(times
 (let ([ht (for/hasheq ([i (in-range K)])
             (values i i))])
   (void
    (for ([i (in-range Q)])
      (for/fold ([v #f]) ([k (in-hash-keys ht)])
        k)))))

'iterate-vals:eq#t
(times
 (let ([ht (for/hasheq ([i (in-range K)])
             (values i #t))])
   (for ([i (in-range Q)])
     (void
      (for/fold ([v #f]) ([v (in-hash-values ht)])
        v)))))

'iterate-vals:eq
(times
 (let ([ht (for/hasheq ([i (in-range K)])
             (values i i))])
   (for ([i (in-range Q)])
     (void
      (for/fold ([v #f]) ([v (in-hash-values ht)])
        v)))))

'iterate-unsafe-keys:eq
(times
 (let ([ht (for/hasheq ([i (in-range J)])
             (values i i))])
   (void
    (for ([i (in-range Q)])
      (for/fold ([v #f]) ([k (in-immutable-hash-keys ht)])
        k)))))

'iterate-unsafe-vals:eq#t
(times
 (let ([ht (for/hasheq ([i (in-range J)])
             (values i #t))])
   (for ([i (in-range Q)])
     (void
      (for/fold ([v #f]) ([v (in-immutable-hash-values ht)])
        v)))))

'iterate-unsafe-vals:eq
(times
 (let ([ht (for/hasheq ([i (in-range J)])
             (values i i))])
   (for ([i (in-range Q)])
     (void
      (for/fold ([v #f]) ([v (in-immutable-hash-values ht)])
        v)))))

'for-each:eq
(times
 (let ([ht (for/hasheq ([i (in-range J)])
             (values i i))])
   (for ([i (in-range Q)])
     (hash-for-each ht (lambda (k v) 'ok)))))
