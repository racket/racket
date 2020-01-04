#lang racket/base
(require "config.rkt")

'eq:keys
(times
 (let ([ht (for/hasheq ([i (in-range 100)])
             (values i i))])
   (void
    (for ([i (in-range Q)])
      (for/fold ([v #f]) ([k (in-hash-keys ht)])
        k)))))

'eq:vals
(times
 (let ([ht (for/hasheq ([i (in-range 100)])
             (values i i))])
   (for ([i (in-range Q)])
     (void
      (for/fold ([v #f]) ([v (in-hash-values ht)])
        v)))))

'eq:keys-unsafe
(times
 (let ([ht (for/hasheq ([i (in-range 100)])
             (values i i))])
   (void
    (for ([i (in-range Q)])
      (for/fold ([v #f]) ([k (in-immutable-hash-keys ht)])
        k)))))

'eq:vals-unsafe
(times
 (let ([ht (for/hasheq ([i (in-range 100)])
             (values i i))])
   (for ([i (in-range Q)])
     (void
      (for/fold ([v #f]) ([v (in-immutable-hash-values ht)])
        v)))))

'eq:for-each
(times
 (let ([ht (for/hasheq ([i (in-range 100)])
             (values i i))])
   (for ([i (in-range Q)])
     (hash-for-each ht (lambda (k v) 'ok)))))
