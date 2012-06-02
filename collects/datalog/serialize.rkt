#lang racket/base
(require racket/contract
         racket/match
         racket/list
         "runtime.rkt"
         "ast.rkt")

(define remove-paths
  (match-lambda
    [(? hash? ht)
     (for/hash ([(k v) (in-hash ht)])
       (values k (remove-paths v)))]
    [(? cons? c)
     (cons (remove-paths (car c))
           (remove-paths (cdr c)))]
    [(? prefab-struct-key s)
     (apply make-prefab-struct
            (prefab-struct-key s)
            (remove-paths (rest (vector->list (struct->vector s)))))]
    [(? path? s)
     #f]
    [x x]))

(define (write-theory t)
  (write (remove-paths t)))

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
