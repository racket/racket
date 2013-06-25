#lang scheme/load

(module source mzscheme

  (define-struct term (posn))           ;; lambda-calc term w/ srcloc info
  (define-struct (var term) (id))
  (define-struct (lam term) (arg body))
  (define-struct (app term) (rator rand))

  (provide (all-defined)))

(module alias typed-scheme

  (define-type-alias Srcloc Any)

  (require-typed-struct term ([posn : Srcloc]) 'source))
