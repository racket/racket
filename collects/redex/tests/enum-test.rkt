#lang racket/base
(require rackunit
         redex
         (for-syntax racket/base))

(define-syntax (try-it stx)
  (syntax-case stx ()
    [(_ N l p)
     (with-syntax ([line (syntax-line stx)])
       #'(for ([i (in-range N)])
           (unless (redex-match
                    l p
                    (generate-term l p #:i-th i))
             (error 'bad-term "line ~a: i=~a" line i))))]))

;; Repeat test
(define-language Rep
  (r (variable variable ...)))

(try-it 100 Rep r)

;; Recursion test
(define-language Λc
  (e (e e)
     (λ (x) e)
     x)
  (x (variable-except λ)))

;; slow: fix dep/enum
(try-it 250 Λc e)
(try-it 24 Λc x)

;; Name test
(define-language Named
  (n (any_1 any_1)))

;; Very slow, to be fixed
(try-it 100 Named n)

(define-language not-SKI
  (x (variable-except s k i)))

(try-it 21 not-SKI x)
