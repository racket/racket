#lang racket/base

;; This module provides type expanders for Typed Racket, which are
;; like match expanders but for abbreviations for types

(provide type-expander
         apply-type-expander
         prop:type-expander
         (rename-out [te? type-expander?]))

;; helper struct for the accessor function
(struct accessor (val ref))

;; te-guard : Any StructInfo -> (U accessor Procedure)
;; a guard function for the type expander property
(define (te-guard val sinfo)
  (cond [(exact-nonnegative-integer? val)
         (accessor val (cadddr sinfo))]
        [(and (procedure? val)
              (procedure-arity-includes? val 1))
         val]
        [else
         (raise-argument-error
          'type-expander-guard
          "(or/c exact-nonnegative-integer? (procedure-arity-includes/c 1))"
          val)]))

;; A property for type expanders, ala prop:match-expander
;;
;; The property accepts either an index into the struct
;; or a procedure of arity 1.
(define-values (prop:type-expander te? te-accessor)
  (make-struct-type-property 'type-expander te-guard))

;; te->expander-proc : TypeExpander -> Procedure
(define (te->expander-proc te)
  (define te-val (te-accessor te))
  (if (accessor? te-val)
      ((accessor-ref te-val) te (accessor-val te-val))
      te-val))

;; A Type-Expander is a (type-expander Procedure)
(struct type-expander (proc) #:property prop:type-expander 0)

;; apply a type-expander in parse-type.rkt
(define (apply-type-expander te stx)
  ((te->expander-proc te) stx))
