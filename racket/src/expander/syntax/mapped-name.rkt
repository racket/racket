#lang racket/base
(require "../common/set.rkt"
         "../common/phase.rkt"
         "syntax.rkt"
         "scope.rkt"
         (submod "scope.rkt" for-debug)
         "binding-table.rkt"
         "fallback.rkt")

(provide syntax-mapped-names
         syntax-mapped-phases)

(define (syntax-mapped-names s phase
                             #:only-interned? [only-interned? #f]
                             #:exactly? [exactly? #f])
  (define s-scs (syntax-scope-set s phase))
  (for/fold ([syms (seteq)]) ([sc (in-set s-scs)])
    (set-union syms
               (binding-table-symbols (scope-binding-table sc) s-scs s null
                                      #:only-interned? only-interned?
                                      #:exactly? exactly?))))

(define (syntax-mapped-phases s)
  (define smss (fallback-first (syntax-shifted-multi-scopes s)))
  (for/fold ([phases (seteqv)]) ([sms (in-set smss)])
    (shifted-multi-scope-add-binding-phases sms phases)))
