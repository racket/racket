#lang racket/base
(require racket/match/match
         (for-syntax racket/base))
(provide (except-out (all-from-out racket/match/match)
                     define-match-expander)
         (rename-out [define-match-expander* define-match-expander]))

(define-for-syntax (no-old-match-form stx)
  (raise-syntax-error
   #f
   "works only for constructor-based `match' form"
   stx))

(define-syntax define-match-expander*
  (syntax-rules ()
    [(_ id expr) (define-match-expander id expr)]
    [(_ id expr expr2) (define-match-expander id
                         expr
                         no-old-match-form
                         (#%expression expr2))]))

