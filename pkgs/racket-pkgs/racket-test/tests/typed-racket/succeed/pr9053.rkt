#lang scheme/load

(module source scheme/base

  (define-struct term (posn))           ;; lambda-calc term w/ srcloc info
  (define-struct (var term) (id))
  (define-struct (lam term) (arg body))
  (define-struct (app term) (rator rand))

  (provide (all-defined-out)))

(module client typed-scheme

  (require-typed-struct term ([posn : Number]) 'source)
  (require-typed-struct var (
                             [id : Symbol])
                        'source))
