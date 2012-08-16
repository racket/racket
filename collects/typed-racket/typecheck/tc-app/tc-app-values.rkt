#lang racket/unit

(require "../../utils/utils.rkt"
         syntax/parse racket/match
         (typecheck signatures tc-app-helper tc-funapp check-below)
         (types abbrev utils)
         (rep type-rep)

         (for-template racket/base))


(import tc-expr^)
(export tc-app-values^)

(define (tc/app-values form expected)
  (syntax-parse form
    #:literals (#%plain-app values call-with-values)
    [(#%plain-app op:special-op args ...) #f]
    ;; call-with-values
    [(#%plain-app call-with-values prod con)
     (match (tc/funapp #'prod #'() (single-value #'prod) null #f)
       [(tc-results: ts fs os)
        (tc/funapp #'con #'(prod) (single-value #'con) (map ret ts fs os) expected)])]
    ;; special case for `values' with single argument
    ;; we just ignore the values, except that it forces arg to return one value
    [(#%plain-app values arg)
     (match expected
      [#f (single-value #'arg)]
      [(tc-result1: tp)
       (single-value #'arg expected)]
      [(tc-results: ts)
       (single-value #'arg) ;Type check the argument, to find other errors
       (tc-error/expr #:return expected
         "wrong number of values: expected ~a but got one"
          (length ts))])]
    ;; handle `values' specially
    [(#%plain-app values . args)
     (match expected
       [(tc-results: ets efs eos)
        (match-let ([(list (tc-result1: ts fs os) ...)
                     (for/list ([arg (syntax->list #'args)]
                                [et ets] [ef efs] [eo eos])
                       (single-value arg (ret et ef eo)))])
          (if (= (length ts) (length ets) (length (syntax->list #'args)))
              (ret ts fs os)
              (tc-error/expr #:return expected "wrong number of values: expected ~a but got ~a"
                             (length ets) (length (syntax->list #'args)))))]
       [_ (match-let ([(list (tc-result1: ts fs os) ...)
                       (for/list ([arg (syntax->list #'args)])
                         (single-value arg))])
            (ret ts fs os))])]
    [_ #f]))
