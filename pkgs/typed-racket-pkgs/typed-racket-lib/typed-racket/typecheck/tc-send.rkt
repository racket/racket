#lang racket/unit

;; This module provides typechecking for `send` method calls

(require "../utils/utils.rkt"
         racket/format
         racket/match syntax/stx
         unstable/match
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
          (define ftype
            (cond [(assq s methods) => cadr]
                  [else (tc-error/expr/fields
                         "send: method not understood by object"
                         "method name" s
                         "object type" obj
                         #:return -Bottom)]))
          (define retval
            (tc/funapp rcvr args (ret ftype) (stx-map tc-expr args) expected))
          (add-typeof-expr form retval)
          retval]
         [(tc-result1: t)
          (int-err "non-symbol methods not supported by Typed Racket: ~a" t)])]
      [(tc-result1: t) (tc-error/expr/fields
                        "send: type mismatch"
                        "expected" "an object"
                        "given" t)]
      [(or (tc-results: ts)
           (as ([ts #f]) (tc-any-results: _)))
       (tc-error/expr/fields
        "send: mismatch in number of values"
        "expected" "1 value of object type"
        "given" (~a (if ts (length ts) "unknown number of") " values"))]))
  (do-check (tc-expr rcvr)))
