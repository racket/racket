#lang racket

(module num-even/odd typed/racket
  (define-type Even (U Null (Pairof Number Odd)))
  (define-type Odd (Pairof Number Even))

  (: even-lst Even)
  (define even-lst '(1 2 3 4))

  (: odd-lst Odd)
  (define odd-lst '(1 2 3)))

(module poly-even/odd typed/racket
  (define-type (Even A) (U Null (Pairof A (Odd A))))
  (define-type (Odd A) (Pairof A (Even A)))

  (: even-lst (Even Integer))
  (define even-lst '(1 2 3 4))

  (: odd-lst (Odd Integer))
  (define odd-lst '(1 2 3))

  (: even->odd (All (A) (A (Even A) -> (Odd A))))
  (define (even->odd elem lst)
    (cons elem lst))

  (provide even->odd Even Odd even-lst odd-lst))

(require 'num-even/odd)
(require 'poly-even/odd)

