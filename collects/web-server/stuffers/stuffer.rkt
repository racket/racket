#lang racket/base
(require racket/contract
         racket/match)

(define-struct stuffer (in out))
(define (stuffer/c dom rng)
  (define in (dom . -> . rng))
  (define in-proc (contract-projection in))
  (define out (rng . -> . dom))  
  (define out-proc (contract-projection out))
  (make-contract
   #:name (build-compound-type-name 'stuffer/c in out)
   #:projection
   (λ (blame)
     (define in-app (in-proc blame))
     (define out-app (out-proc blame))
     (λ (val)
       (unless (stuffer? val)
         (raise-blame-error
          blame
          val
          "expected <stuffer>, given: ~e"
          val))
       (make-stuffer
        (in-app (stuffer-in val))
        (out-app (stuffer-out val)))))
   #:first-order stuffer?))

(define id-stuffer
  (make-stuffer 
   (lambda (v) v)
   (lambda (v) v)))

(define (stuffer-compose g f)
  (make-stuffer
   (lambda (v)
     ((stuffer-in g) ((stuffer-in f) v)))
   (lambda (v)
     ((stuffer-out f) ((stuffer-out g) v)))))

(define (stuffer-sequence f g)
  (stuffer-compose g f))

(define (stuffer-if c f)
  (make-stuffer
   (lambda (v)
     (if (c v)
         (bytes-append #"1" ((stuffer-in f) v))
         (bytes-append #"0" v)))
   (lambda (tv)
     (define tag (subbytes tv 0 1))
     (define v (subbytes tv 1))
     (if (bytes=? tag #"1")
         ((stuffer-out f) v)
         v))))

(define (stuffer-chain . ss)
  (match ss
    [(list)
     id-stuffer]
    [(list-rest f ss)
     (cond
       [(stuffer? f)     
        (stuffer-sequence
         f (apply stuffer-chain ss))]
       [(procedure? f)
        (stuffer-if 
         f (apply stuffer-chain ss))])]))

(define-values (alpha beta gamma) (values any/c any/c any/c))
(provide/contract
 [struct stuffer
         ([in (any/c . -> . any/c)]
          [out (any/c . -> . any/c)])]
 [stuffer/c (any/c any/c . -> . contract?)]
 [id-stuffer (stuffer/c alpha alpha)]
 [stuffer-compose ((stuffer/c beta gamma) (stuffer/c alpha beta) . -> . (stuffer/c alpha gamma))]
 [stuffer-sequence ((stuffer/c alpha beta) (stuffer/c beta gamma) . -> . (stuffer/c alpha gamma))]
 [stuffer-if ((bytes? . -> . boolean?) (stuffer/c bytes? bytes?) . -> . (stuffer/c bytes? bytes?))]
 [stuffer-chain (() () #:rest (listof (or/c stuffer? (bytes? . -> . boolean?))) . ->* . stuffer?)])
