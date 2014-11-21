#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match unstable/sequence unstable/syntax
         (typecheck signatures tc-funapp)
         (types utils)

         (for-label racket/base))


(import tc-expr^ tc-app^)
(export tc-app-values^)

(define-literal-set values-literals #:for-label (values call-with-values))

(define-tc/app-syntax-class (tc/app-values expected)
  #:literal-sets (values-literals)
  ;; call-with-values
  (pattern (call-with-values prod con)
    (match (tc/funapp #'prod #'() (tc-expr/t #'prod) null #f)
      [(tc-results: ts fs os)
       (tc/funapp #'con #'(prod) (tc-expr/t #'con) (map ret ts fs os) expected)]
      [(tc-results: ts fs os drest dbound)
       (tc-error/expr "`call-with-values` with ... is not supported")]
      [(tc-any-results: _)
       (tc/app-regular this-syntax expected)]))
  ;; special case for `values' with single argument
  ;; we just ignore the values, except that it forces arg to return one value
  (pattern (values arg)
    (match expected
     [(or #f (tc-any-results: _)) (single-value #'arg)]
     [(tc-result1: tp)
      (single-value #'arg expected)]
     [(tc-results: ts)
      (single-value #'arg) ;Type check the argument, to find other errors
      (tc-error/expr
        "wrong number of values: expected ~a but got one"
         (length ts))]
     ;; match polydots case and error
     [(tc-results: ts _ _ dty dbound)
      (single-value #'arg)
      (tc-error/expr
        "Expected ~a ..., but got only one value" dty)]))
  ;; handle `values' specially
  (pattern (values . args)
    (match expected
      [(tc-results: ets efs eos)
       (match-let ([(list (tc-result1: ts fs os) ...)
                    (for/list ([arg (in-syntax #'args)]
                               [et (in-list ets)]
                               [ef (in-list efs)]
                               [eo (in-list eos)])
                      (single-value arg (ret et ef eo)))])
         (if (= (length ts) (length ets) (syntax-length #'args))
             (ret ts fs os)
             (tc-error/expr "wrong number of values: expected ~a but got ~a"
                            (length ets) (syntax-length #'args))))]
      [_ (match-let ([(list (tc-result1: ts fs os) ...)
                      (for/list ([arg (in-syntax #'args)])
                        (single-value arg))])
           (ret ts fs os))])))
