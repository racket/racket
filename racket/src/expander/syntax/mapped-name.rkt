#lang racket/base
(require "../common/set.rkt"
         "syntax.rkt"
         "scope.rkt"
         (submod "scope.rkt" for-debug)
         "binding-table.rkt"
         "fallback.rkt"
         "../common/phase.rkt")

(provide syntax-mapped-names
         syntax-mapped-symbols)

(define (syntax-mapped-names s phase)
  (define s-scs (syntax-scope-set s phase))
  (for/fold ([syms (seteq)]) ([sc (in-set s-scs)])
    (set-union syms
               (binding-table-symbols (scope-binding-table sc) s-scs s null))))

(define (syntax-mapped-symbols s)
  (define smss (fallback-first (syntax-shifted-multi-scopes s)))
  (define phases
    (for/fold ([phases (seteqv)])
              ([sms (in-set smss)])
      (define ms (shifted-multi-scope-multi-scope sms))
      (define phase (shifted-multi-scope-phase sms))
      (cond
        [(phase? phase)
         (for/fold ([phases phases])
                   ([ph (in-hash-keys (unbox (multi-scope-scopes ms)))]
                    #:unless (label-phase? ph))
           (set-add phases (phase- phase ph)))]
        [else
         phases])))
  (for*/list ([ph (in-set (set-add phases #f))]
              [s (in-value (syntax-mapped-names s ph))]
              #:unless (set-empty? s))
    (cons ph (set->list s))))
