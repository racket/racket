#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/binding.rkt"
         "../syntax/scope.rkt"
         "../namespace/core.rkt"
         "context.rkt"
         "../syntax/error.rkt")

(provide prop:expansion-contexts

         not-in-this-expand-context?
         avoid-current-expand-context)

(define-values (prop:expansion-contexts expansion-contexts? expansion-contexts-ref)
  (make-struct-type-property 'expansion-contexts
                             (lambda (v info)
                               (unless (and (list? v)
                                            (for/and ([s (in-list v)])
                                              (memq s '(expression top-level module module-begin definition-context))))
                                 (raise-argument-error 'guard-for-prop:expansion-contexts
                                                       "(listof (or/c 'expression 'top-level 'module 'module-begin 'definition-context))"
                                                       v))
                               v)))



(define (not-in-this-expand-context? t ctx)
  (and (expansion-contexts? t)
       (not (memq (context->symbol (expand-context-context ctx))
                  (expansion-contexts-ref t)))))

(define (context->symbol context)
  (if (symbol? context)
      context
      'definition-context))

(define (avoid-current-expand-context s t ctx)
  (define (wrap sym)
     (datum->syntax #f (list (syntax-shift-phase-level
                              (datum->syntax core-stx sym)
                              (expand-context-phase ctx))
                             s)))
  (define (fail)
    (raise-syntax-error
     #f
     (format "not allowed in context\n  expansion context: ~a"
             (context->symbol (expand-context-context ctx)))
     s))
  (case (context->symbol (expand-context-context ctx))
    [(module-begin) (wrap 'begin)]
    [(module top-level definition-context)
     (if (memq 'expression (expansion-contexts-ref t))
         (wrap '#%expression)
         (fail))]
    [else (fail)]))
