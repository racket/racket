#lang racket/base

(require
  racket/match
  racket/contract
  (except-in "structures.rkt" recursive-contract)
  "combinators/parametric.rkt"
  "combinators/structural.rkt")

(provide
  (contract-out
    [parametric-check (static-contract? . -> . boolean?)]))

(define (parametric-check sc)
  (let/ec exit
    (define (recur sc variance)
      (match sc
        [(or/sc: elems ...) (=> continue)
         (match elems
           [(list-no-order (parametric-var/sc: _) (parametric-var/sc: _) others ...)
            (exit #t)]
           [else (continue)])]
        [else
          (sc-traverse sc recur)]))
    (recur sc 'covariant)
    #f))
