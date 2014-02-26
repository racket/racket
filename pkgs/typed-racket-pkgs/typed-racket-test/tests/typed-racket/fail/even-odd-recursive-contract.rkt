#;
(exn-pred #rx"blaming: \\(interface for even->odd\\)")
#lang racket/load

;; Test mutually recursive type alias contract generation
;; with polymorphism

(module untyped racket
  (define (even->odd elem lst) (cons 3 lst))
  (provide even->odd))

(module poly-even/odd typed/racket
  (define-type (Even A) (U Null (Pairof A (Odd A))))
  (define-type (Odd A) (Pairof A (Even A)))

  (: even-lst (Even Integer))
  (define even-lst '(1 2 3 4))

  (: odd-lst (Odd Integer))
  (define odd-lst '(1 2 3))

  (require/typed 'untyped
                 [even->odd (All (A) (A (Even A) -> (Odd A)))])

  (even->odd 3 even-lst) )

(require 'poly-even/odd)

