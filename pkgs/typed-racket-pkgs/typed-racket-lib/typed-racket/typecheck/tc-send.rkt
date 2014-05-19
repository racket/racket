#lang racket/unit

;; This module provides typechecking for `send` method calls

(require "../utils/utils.rkt"
         racket/match syntax/stx
         (typecheck signatures tc-funapp tc-metafunctions)
         (types base-abbrev resolve utils type-table)
         (rep type-rep)
         (utils tc-utils))

(import tc-expr^)
(export tc-send^)

(define (tc/send form rcvr method args [expected #f])
  ;; do-check : Type/c -> tc-results/c
  (define (do-check rcvr-type)
    (match rcvr-type
      [(Instance: (? needs-resolving? type))
       (do-check (make-Instance (resolve type)))]
      [(and obj (Instance: (Class: _ _ _ methods _ _)))
       (match (tc-expr/t method)
         [(Value: (? symbol? s))
          (define ftype
            (cond [(assq s methods) => cadr]
                  [else (tc-error/expr/fields
                         "send: method not understood by object"
                         "method name" s
                         "object type" obj
                         #:return -Bottom)]))
          (tc/funapp rcvr args ftype (stx-map tc-expr args) expected)]
         [_ (int-err "non-symbol methods not supported by Typed Racket: ~a"
                     rcvr-type)])]
      ;; union of objects, check pointwise and union the results
      [(Union: (list (and objs (Instance: _)) ...))
       (merge-tc-results
        (for/list ([obj (in-list objs)])
          (do-check obj)))]
      [_ (tc-error/expr/fields
          "send: type mismatch"
          "expected" "an object"
          "given" rcvr-type)]))
  (define final-ret (do-check (tc-expr/t rcvr)))
  (add-typeof-expr form final-ret)
  final-ret)
