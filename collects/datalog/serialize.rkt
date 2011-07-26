#lang racket/base
(require racket/contract
         racket/match
         racket/list
         "runtime.rkt")

(define remove-stx-objs
  (match-lambda
    [(? hash? ht)
     (for/hash ([(k v) (in-hash ht)])
       (values k (remove-stx-objs v)))]
    [(? cons? c)
     (cons (remove-stx-objs (car c))
           (remove-stx-objs (cdr c)))]
    [(? prefab-struct-key s)
     (apply make-prefab-struct
            (prefab-struct-key s)
            (remove-stx-objs (rest (vector->list (struct->vector s)))))]
    [(? syntax? s)
     #f]
    [x x]))

(define (write-theory t)
  (write (remove-stx-objs t)))

(define (hash->hash! ht)
  (define ht! (make-hash))
  (for ([(k v) (in-hash ht)])
    (hash-set! ht! k v))
  ht!)

(define (read-theory)
  (hash->hash! (read)))

(provide/contract
 [write-theory (-> theory/c void)]
 [read-theory (-> theory/c)])