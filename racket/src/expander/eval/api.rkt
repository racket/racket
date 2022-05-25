#lang racket/base
(require (prefix-in direct: "main.rkt")
         (prefix-in direct: "../namespace/api.rkt")
         "../syntax/api.rkt"
         "../syntax/taint.rkt"
         "../namespace/namespace.rkt"
         "../common/contract.rkt"
         "parameter.rkt"
         "../eval/protect.rkt")

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

(define/who (expand s [insp (current-code-inspector)])
  (check who inspector? insp)
  (maybe-taint-expanded
   insp
   (direct:expand (intro s) (current-namespace) #t)))

(define/who (expand-syntax s [insp (current-code-inspector)])
  (check who syntax? s)
  (check who inspector? insp)
  (maybe-taint-expanded
   insp
   (direct:expand s (current-namespace) #t)))

(define/who (expand-once s [insp (current-code-inspector)])
  (check who inspector? insp)
  (maybe-taint-expanded
   insp
   (direct:expand-once (intro s))))

(define/who (expand-syntax-once s [insp (current-code-inspector)])
  (check who syntax? s)
  (check who inspector? insp)
  (maybe-taint-expanded
   insp
   (direct:expand-once s)))

(define/who (expand-to-top-form s [insp (current-code-inspector)])
  (check who inspector? insp)
  (maybe-taint-expanded
   insp
   (direct:expand-to-top-form (intro s))))

(define/who (expand-syntax-to-top-form s [insp (current-code-inspector)])
  (check who syntax? s)
  (check who inspector? insp)
  (maybe-taint-expanded
   insp
   (direct:expand-to-top-form s)))


(define (intro given-s [ns (current-namespace)])
  (define s (if (syntax? given-s) given-s (datum->syntax #f given-s)))
  (direct:namespace-syntax-introduce s ns))

(define (maybe-taint-expanded insp s)
  (if (eq? insp initial-code-inspector)
      s
      (syntax-taint s)))
