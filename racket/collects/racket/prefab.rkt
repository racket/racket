#lang racket/base

(provide immutable-prefab-struct-key
         prefab-key-all-fields-immutable?)

(define (immutable-prefab-struct-key v)
  (define k (prefab-struct-key v))
  (and k
       (all-fields-immutable? k)
       k))

(define (prefab-key-all-fields-immutable? k)
  (unless (prefab-key? k)
    (raise-argument-error 'prefab-key-all-fields-immutable? "prefab-key?" k))
  (all-fields-immutable? k))

(define (all-fields-immutable? k)
  (or (symbol? k)
      (null? k)
      (let* ([rk (cdr k)] ; skip name
             [rk (if (and (pair? rk)
                          (exact-integer? (car rk)))
                     (cdr rk) ; skip init count
                     rk)]
             [rk (if (and (pair? rk)
                          (pair? (car rk)))
                     (if (zero? (caar rk))
                         (cdr rk) ; skip zero auto count
                         (cons '#(1) (cdr rk))) ; reflect mutable auto field
                     rk)])
        (if (and (pair? rk)
                 (vector? (car rk)))
            (if (zero? (vector-length (car rk)))
                (all-fields-immutable? (cdr rk))
                #f)
            (all-fields-immutable? rk)))))
