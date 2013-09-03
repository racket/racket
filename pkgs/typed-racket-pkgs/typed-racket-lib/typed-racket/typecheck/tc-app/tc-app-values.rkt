#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match unstable/sequence unstable/syntax
         (typecheck signatures tc-funapp)
         (types utils)

         (for-template racket/base))


(import tc-expr^)
(export tc-app-values^)

(define-tc/app-syntax-class (tc/app-values expected)
  #:literals (values call-with-values)
  ;; call-with-values
  (pattern (call-with-values prod con)
    (match (tc/funapp #'prod #'() (single-value #'prod) null #f)
      [(tc-results: ts fs os)
       (tc/funapp #'con #'(prod) (single-value #'con) (map ret ts fs os) expected)]))

  ;; special case for `values' with single argument
  ;; we just ignore the values, except that it forces arg to return one value
  (pattern (values arg)
    (match expected
     [(or #f (tc-any-results:)) (single-value #'arg)]
     [(tc-result1: tp)
      (single-value #'arg expected)]
     [(tc-results: ts)
      (single-value #'arg) ;Type check the argument, to find other errors
      (tc-error/expr #:return expected
        "wrong number of values: expected ~a but got one"
         (length ts))]
     ;; match polydots case and error
     [(tc-results: ts _ _ dty dbound)
      (single-value #'arg)
      (tc-error/expr #:return expected
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
             (tc-error/expr #:return expected "wrong number of values: expected ~a but got ~a"
                            (length ets) (syntax-length #'args))))]
      [_ (match-let ([(list (tc-result1: ts fs os) ...)
                      (for/list ([arg (in-syntax #'args)])
                        (single-value arg))])
           (ret ts fs os))])))
