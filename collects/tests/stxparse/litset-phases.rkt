#lang racket/load

(module a racket
  (require syntax/parse)
  (define-literal-set lits (begin))
  (provide lits))

(module b racket
  (require (for-syntax 'a syntax/parse))
  (require (for-syntax syntax/parse/private/runtime))
  (define-syntax (snarf stx)
    ;;(printf "slpl of snarf: ~s\n" (syntax-local-phase-level))
    (syntax-parse stx
      #:literal-sets (lits)
      [(snarf (begin e)) #'e]))
  (provide snarf))

(module c racket
  (require (for-syntax 'b racket/base))
  (begin-for-syntax
    (displayln (snarf (begin 5)))))
