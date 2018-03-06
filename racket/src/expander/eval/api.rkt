#lang racket/base
(require (prefix-in direct: "main.rkt")
         (prefix-in direct: "../namespace/api.rkt")
         "../syntax/api.rkt"
         "../namespace/namespace.rkt"
         "../common/contract.rkt"
         "parameter.rkt")

;; These wrappers implement the protocol for whether to use
;; `namespace-synatx-introduce` on the argument to `eval`, etc.

(provide eval
         eval-syntax
         
         compile
         compile-syntax
         
         expand
         expand-syntax
         
         expand-to-top-form
         expand-syntax-to-top-form
         
         expand-once
         expand-syntax-once)

(define/who eval
  (case-lambda
    [(s) ((current-eval) (intro s))]
    [(s ns)
     (check who namespace? ns)
     (parameterize ([current-namespace ns])
       ((current-eval) (intro s ns)))]))

(define/who eval-syntax
  (case-lambda
    [(s)
     (check who syntax? s)
     ((current-eval) s)]
    [(s ns)
     (check who syntax? s)
     (check who namespace? ns)
     (parameterize ([current-namespace ns])
       ((current-eval) s))]))

(define (compile s)
  ((current-compile) (intro s) #f))

(define/who (compile-syntax s)
  (check who syntax? s)
  ((current-compile) s #f))

(define (expand s)
  (direct:expand (intro s) (current-namespace) #t))

(define/who (expand-syntax s)
  (check who syntax? s)
  (direct:expand s (current-namespace) #t))

(define (expand-once s)
  (direct:expand-once (intro s)))

(define/who (expand-syntax-once s)
  (check who syntax? s)
  (direct:expand-once s))

(define (expand-to-top-form s)
  (direct:expand-to-top-form (intro s)))

(define/who (expand-syntax-to-top-form s)
  (check who syntax? s)
  (direct:expand-to-top-form s))


(define (intro given-s [ns (current-namespace)])
  (define s (if (syntax? given-s) given-s (datum->syntax #f given-s)))
  (direct:namespace-syntax-introduce s ns))
