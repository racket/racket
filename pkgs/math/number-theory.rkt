#lang racket/base

(require typed/untyped-utils
         "private/number-theory/divisibility.rkt"
         "private/number-theory/modular-arithmetic.rkt"
         (except-in "private/number-theory/number-theory.rkt" prev-prime next-prime)
         (except-in "private/number-theory/factorial.rkt" factorial permutations)
         "private/number-theory/bernoulli.rkt"
         "private/number-theory/eulerian-number.rkt"
         "private/number-theory/farey.rkt"
         "private/number-theory/fibonacci.rkt"
         "private/number-theory/partitions.rkt"
         "private/number-theory/polygonal.rkt"
         "private/number-theory/primitive-roots.rkt"
         "private/number-theory/quadratic.rkt"
         "private/number-theory/quadratic-residues.rkt"
         "private/number-theory/tangent-number.rkt")

(require/untyped-contract
 "private/number-theory/factorial.rkt"
 [factorial  (Integer -> Positive-Integer)]
 [permutations  (Integer Integer -> Natural)])

(require/untyped-contract
 "private/number-theory/binomial.rkt"
 [binomial  (Integer Integer -> Natural)])

(require/untyped-contract
 "private/number-theory/number-theory.rkt"
 [next-prime (Integer -> Integer)]
 [prev-prime (Integer -> Integer)])

(provide (all-from-out
          "private/number-theory/divisibility.rkt"
          "private/number-theory/modular-arithmetic.rkt"
          "private/number-theory/number-theory.rkt"
          "private/number-theory/factorial.rkt"
          "private/number-theory/bernoulli.rkt"
          "private/number-theory/eulerian-number.rkt"
          "private/number-theory/farey.rkt"
          "private/number-theory/fibonacci.rkt"
          "private/number-theory/partitions.rkt"
          "private/number-theory/polygonal.rkt"
          "private/number-theory/primitive-roots.rkt"
          "private/number-theory/quadratic.rkt"
          "private/number-theory/quadratic-residues.rkt"
          "private/number-theory/tangent-number.rkt")
         next-prime prev-prime
         factorial permutations
         binomial)
