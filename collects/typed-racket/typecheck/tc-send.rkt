#lang racket/unit


(require "../utils/utils.rkt"
         racket/match 
         (typecheck signatures check-below tc-funapp)
         (types utils union type-table)
         (rep type-rep) 
         (utils tc-utils))

(import tc-expr^)
(export tc-send^)

(define (tc/send form rcvr method args [expected #f])
  (match (tc-expr rcvr)
    [(tc-result1: (Instance: (and c (Class: _ _ methods))))
     (match (tc-expr method)
       [(tc-result1: (Value: (? symbol? s)))
        (let* ([ftype (cond [(assq s methods) => cadr]
                            [else (tc-error/expr "send: method ~a not understood by class ~a" s c)])]
               [ret-ty (tc/funapp rcvr args (ret ftype) (map tc-expr (syntax->list args)) expected)]
               [retval (cond-check-below ret-ty expected)])
          (add-typeof-expr form retval)
          retval)]
       [(tc-result1: t) (int-err "non-symbol methods not supported by Typed Racket: ~a" t)])]
    [(tc-result1: t) (tc-error/expr #:return (or expected (ret (Un))) "send: expected a class instance, got ~a" t)]))

