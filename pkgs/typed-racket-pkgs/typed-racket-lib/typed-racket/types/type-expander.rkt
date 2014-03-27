#lang racket/base

;; This module provides type expanders for Typed Racket, which are
;; like match expanders but for abbreviations for types

(require (for-syntax racket/base
                     syntax/parse))

(provide define-type-expander
         (for-syntax apply-type-expander
                     prop:type-expander
                     (rename-out [te? type-expander?])))

(begin-for-syntax
  ;; helper struct for the accessor function
  (struct accessor (val ref))

  ;; te-guard : Any StructInfo -> Boolean?
  ;; a guard function for the type expander property
  (define (te-guard val sinfo)
    (cond [(exact-nonnegative-integer? val)
           (accessor val (cadddr sinfo))]
          [(procedure? val) val]
          [else
           (raise-argument-error 'type-expander-guard
                                 "an exact non-negative integer or procedure"
                                 val)]))
  
  ;; A property for type expanders, ala prop:match-expander
  (define-values (prop:type-expander te? te-accessor)
    (make-struct-type-property 'type-expander te-guard))

  ;; te->expander-proc : TypeExpander -> Procedure
  (define (te->expander-proc te)
    (define te-val (te-accessor te))
    (if (accessor? te-val)
        ((accessor-ref te-val) te (accessor-val te-val))
        te-val))

  ;; A Type-Expander is a (type-expander Procedure)
  (struct type-expander (proc)
    #:property prop:set!-transformer
    (λ (te stx)
      (define transformer (type-expander-proc te))
      (if (set!-transformer? transformer)
          ((set!-transformer-procedure transformer) stx)
          (syntax-case stx (set!)
            [(set! . _)
             (raise-syntax-error #f "cannot mutate type identifier" stx)]
            [_ (transformer stx)])))
    #:property prop:type-expander 0)

  ;; apply a type-expander in parse-type.rkt
  (define (apply-type-expander te-id stx)
    (define te (syntax-local-value te-id))
    ((te->expander-proc te) stx)))

;; macro for defining type expanders
(define-syntax (define-type-expander stx)
  (syntax-parse stx
    [(_ ?name:id ?expander-expr)
     #'(define-syntax ?name (type-expander ?expander-expr))]))
