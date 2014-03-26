#;
(exn-pred 2)
#lang typed/racket/base

(define-type T (Rec T (U (Pair String T) (Pair Char T))))
(define-type S (Rec S (Pair (U String Char) S)))

(: f (S -> S))
(: g (T -> T))

(define (f x) (cons "string" (cons #\a x)))
(define g f)
