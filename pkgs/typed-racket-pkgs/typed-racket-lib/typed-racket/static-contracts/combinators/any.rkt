#lang racket/base

(require "../structures.rkt" "../constraints.rkt"
         racket/match
         (except-in racket/contract recursive-contract)
         (for-template racket/base racket/contract/base)
         (for-syntax racket/base racket/syntax syntax/parse))

(provide
  (contract-out
    [any/sc static-contract?])
  any/sc:)


;;Printing
(define (any-write-proc v port mode)
  (if (equal? mode 0)
      (display "any/sc" port)
      (display "#<any/sc>" port)))

(struct any-combinator combinator ()
        #:methods gen:sc
          [(define (sc-map v f) v)
           (define (sc->contract v f) #'any/c)
           (define (sc->constraints v f) (simple-contract-restrict 'flat))]
        #:methods gen:custom-write [(define write-proc any-write-proc)])

(define-match-expander any/sc:
  (syntax-parser
    [(_) #'(? any-combinator?)]))

(define any/sc (any-combinator null))

