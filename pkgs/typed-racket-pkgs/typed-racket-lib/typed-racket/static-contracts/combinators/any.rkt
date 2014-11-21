#lang racket/base

;; Static contract for any/c.
;; Allows optimizations as many combinators can be simplified if their arguments are any/sc
;; Ex: (listof/sc any/sc) => list?/sc

(require "../structures.rkt" "../constraints.rkt"
         racket/match
         racket/contract
         (for-template racket/base racket/contract/base)
         (for-syntax racket/base syntax/parse))

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
        #:transparent
        #:methods gen:sc
          [(define (sc-map v f) v)
           (define (sc-traverse v f) (void))
           (define (sc->contract v f) #'any/c)
           (define (sc->constraints v f) (simple-contract-restrict 'flat))]
        #:methods gen:custom-write [(define write-proc any-write-proc)])

(define-match-expander any/sc:
  (syntax-parser
    [(_) #'(? any-combinator?)]))

(define any/sc (any-combinator null))

