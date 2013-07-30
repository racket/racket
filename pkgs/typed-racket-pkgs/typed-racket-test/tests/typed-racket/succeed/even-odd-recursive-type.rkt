#lang racket

(require rackunit)

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

;; make sure it works in a let
(module let-even/odd typed/racket
  (let ()
    (define-type (Even A) (U Null (Pairof A (Odd A))))
    (define-type (Odd B) (Pairof B (Even B)))

    (: even-lst (Even Integer))
    (define even-lst '(1 2 3 4))

    (: odd-lst (Odd Integer))
    (define odd-lst '(1 2 3))

    (cons 3 even-lst)))

(module even/odd* typed/racket
  ;; weird variant that alternates types between
  ;; Even and Odd
  (define-type (Even A B) (U Null (Pairof A (Odd A B))))
  (define-type (Odd A B) (Pairof B (Even A B)))

  (: even-lst (Even Integer String))
  (define even-lst '(1 "b" 3 "a"))

  (: odd-lst (Odd Integer String))
  (define odd-lst '("b" 2 "a"))

  ;; specialized for more interesting contract
  (: even->odd (String (Even Integer String) -> (Odd Integer String)))
  (define (even->odd elem lst) (cons elem lst))

  (provide even-lst odd-lst even->odd))

(require (prefix-in a: 'num-even/odd))
(require (prefix-in b: 'poly-even/odd))
(require (prefix-in c: 'let-even/odd))
(require (prefix-in d: 'even/odd*))

;; make sure contract generation on even/odd* worked
(cons 3 d:odd-lst)
(check-equal? (d:even->odd "c" d:even-lst) '("c" 1 "b" 3 "a"))
(check-exn exn:fail:contract? (λ () (d:even->odd 1 d:even-lst)))

(b:even->odd "c" b:even-lst)
(check-exn exn:fail:contract? (λ () (b:even->odd "c" b:odd-lst)))

