#lang racket/base
(require "../common/set.rkt"
         "syntax.rkt"
         "scope.rkt"
         (submod "scope.rkt" for-debug)
         "binding-table.rkt")

(provide syntax-mapped-names)

(define (syntax-mapped-names s phase)
  (define s-scs (syntax-scope-set s phase))
  (for/fold ([syms (seteq)]) ([sc (in-set s-scs)])
    (set-union syms
               (binding-table-symbols (scope-binding-table sc) s-scs s null))))
