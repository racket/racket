#lang racket/unit


(require "../utils/utils.rkt"
         racket/match syntax/stx
         (typecheck signatures tc-funapp)
         (types base-abbrev resolve utils type-table)
         (rep type-rep) 
         (utils tc-utils))

(import tc-expr^)
(export tc-send^)

(define (tc/send form rcvr method args [expected #f])
  (define (do-check rcvr-type)
   (match rcvr-type
     [(tc-result1: (Instance: (? needs-resolving? type)))
      (do-check (ret (make-Instance (resolve type))))]
     [(tc-result1: (and obj (Instance: (Class: _ _ _ methods _ _))))
      (match (tc-expr method)
        [(tc-result1: (Value: (? symbol? s)))
         (let* ([ftype (cond [(assq s methods) => cadr]
                             [else (tc-error/expr/fields "send: method not understood by object"
                                                         "method name" s
                                                         "object type" obj
                                                         #:return -Bottom)])]
                [retval (tc/funapp rcvr args (ret ftype) (stx-map tc-expr args) expected)])
           (add-typeof-expr form retval)
           retval)]
        [(tc-result1: t) (int-err "non-symbol methods not supported by Typed Racket: ~a" t)])]
     [(tc-result1: t) (tc-error/expr/fields
                        "send: type mismatch"
                        "expected" "an object"
                        "given" t)]))
  (do-check (tc-expr rcvr)))

