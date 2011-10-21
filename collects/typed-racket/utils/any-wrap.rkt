#lang racket/base

(require racket/match racket/vector racket/contract/base racket/contract/combinator)

(define-struct any-wrap (val)
  #:property prop:custom-write
  (lambda (v p write?)
    (fprintf p "#<Typed Value: ~a>" (any-wrap-val v))))

(define undef (letrec ([x x]) x))

(define (traverse wrap?)
  (define (t v)
    (match v
      [(? (lambda (e) (and (any-wrap? e) (not wrap?)))) (any-wrap-val v)]
      [(? (lambda (e)
            (or (number? e) (string? e) (char? e) (symbol? e)
                (null? e) (regexp? e) (eq? undef e)
                (keyword? e) (bytes? e) (boolean? e) (void? e))))
       v]
      [(cons x y) (cons (t x) (t y))]
      [(and (? immutable?) (? vector?)) (vector-map t v)]
      [(and (? immutable?) (box v)) (box (t v))]
      [(and (? immutable?) (? hash? v))
       ((cond [(hash-eq? v) make-immutable-hasheq]
              [(hash-eqv? v) make-immutable-hasheqv]
              [else make-immutable-hash])
        (for/list ([(k v) (in-hash v)])
          (cons (t k) (t v))))]
      #; ;; need to check immutablity
      [(? prefab-struct-key)
       (let* ([k (prefab-struct-key v)]
              [vals (struct->vector v)])
         (apply make-prefab-struct k (for/list ([i (in-vector vals 1)]) i)))]
      [_ (if wrap? (make-any-wrap v) v)]))
  t)

(define any-wrap/c
  (make-contract
   #:name 'Any
   #:projection (compose traverse blame-original?)))

(provide any-wrap/c)
