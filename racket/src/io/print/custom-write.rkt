#lang racket/base

(provide prop:custom-write
         custom-write?
         custom-write-accessor

         prop:custom-print-quotable
         custom-print-quotable?
         custom-print-quotable-accessor)

(define-values (prop:custom-write custom-write? custom-write-accessor)
  (make-struct-type-property 'custom-write
                             (lambda (v info)
                               (unless (and (procedure? v)
                                            (procedure-arity-includes? v 3))
                                 (raise-argument-error
                                  'guard-for-prop:custom-write
                                  "(procedure-arity-includes?/c 3)"
                                  v))
                               v)))

(define-values (prop:custom-print-quotable custom-print-quotable? custom-print-quotable-accessor)
  (make-struct-type-property 'custom-print-quotable
                             (lambda (v info)
                               (unless (or (eq? v 'self) (eq? v 'never) (eq? v 'maybe) (eq? v 'always))
                                 (raise-argument-error
                                  'guard-for-prop:custom-print-quotable
                                  "(or/c 'self 'never 'maybe 'always)"
                                  v))
                               v)))
