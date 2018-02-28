#lang racket/base
(require "../common/phase.rkt"
         "../common/module-path.rkt"
         "../syntax/scope.rkt"
         "../syntax/module-binding.rkt"
         "../expand/parsed.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../host/linklet.rkt"
         "protect.rkt")

;; Instead of going through all the work to compile, optimize, and
;; evaluate a compile-time expression, it might be easier and faster
;; to evaluate the expression directly.

(provide can-direct-eval?
         direct-eval)

(define not-available (gensym 'not-available))
(define (get-not-available) not-available)

(define (can-direct-eval? p ns self-mpi)
  (cond
   [(parsed-app? p)
    (and (can-direct-eval? (parsed-app-rator p) ns self-mpi)
         (for/and ([r (in-list (parsed-app-rands p))])
           (can-direct-eval? r ns self-mpi)))]
   [(parsed-id? p) (not (eq? (get-id-value p ns self-mpi) not-available))]
   [(parsed-quote? p) #t]
   [(parsed-quote-syntax? p) #t]
   [else #f]))

(define (direct-eval p ns self-mpi)
  (cond
   [(parsed-app? p)
    (apply (direct-eval (parsed-app-rator p) ns self-mpi)
           (for/list ([r (in-list (parsed-app-rands p))])
             (direct-eval r ns self-mpi)))]
   [(parsed-id? p) (get-id-value p ns self-mpi)]
   [(parsed-quote? p) (parsed-quote-datum p)]
   [(parsed-quote-syntax? p) (parsed-quote-syntax-datum p)]
   [else #f]))

;; Return `not-available` if the value is not readily available.
(define (get-id-value p ns self-mpi)
  (define b (parsed-id-binding p))
  (cond
   [(parsed-primitive-id? p)
    (hash-ref (primitive-table '#%kernel)
              (module-binding-sym b)
              get-not-available)]
   [(or (parsed-top-id? p)
        (not b)
        (eq? self-mpi (module-binding-module b)))
    (namespace-get-variable
     ns
     (if b (module-binding-phase b) (namespace-phase ns))
     (if b (module-binding-sym b) (syntax-e (parsed-s p)))
     get-not-available)]
   [else
    (define mi
      (namespace->module-instance ns
                                  (module-path-index-resolve (module-binding-module b))
                                  (phase- (namespace-phase ns) (module-binding-phase b))))
    (cond
     [(not mi) not-available]
     [(check-single-require-access mi
                                   (module-binding-phase b)
                                   (module-binding-sym b)
                                   (module-binding-extra-inspector b))
      (namespace-get-variable (module-instance-namespace mi)
                              (module-binding-phase b)
                              (module-binding-sym b)
                              get-not-available)]
     [else not-available])]))
