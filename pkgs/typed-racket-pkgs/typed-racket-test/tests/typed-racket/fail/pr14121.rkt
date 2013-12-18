#;
(exn-pred #rx"expected: Number\n  given: \\(U Integer String\\)")
#lang racket/load

;; Test for PR 14121
;; Top-level variables cannot be refined with occurrence typing
;; because it's not possible to detect future set!s reliably

(require typed/racket)

(: x (U Integer String))
(define x 3)
(define f (if (integer? x) (lambda () (add1 x)) (lambda () 3)))

