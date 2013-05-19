#lang racket/base
(require rackunit
         redex
         (for-syntax racket/base))

(define-syntax (try-it stx)
  (syntax-case stx ()
    [(_ N l p)
     (with-syntax ([line (syntax-line stx)])
       #'(test-begin
          (for ([i (in-range N)])
            (check-not-exn
             (λ ()
                (unless (redex-match
                         l p
                         (generate-term l p #:i-th i))
                  (error 'bad-term "line ~a: i=~a" line i)))))))]))

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
  (n (number_1 number_1)))

;; Very slow, to be fixed
(try-it 100 Named n)

(define-language not-SKI
  (y x
     s
     k
     i)
  (x (variable-except s k i)))

(try-it 22 not-SKI x)
(try-it 25 not-SKI y)

(define-language λv
  (e (e e ...)
     (if0 e e e)
     x
     v)
  (v (λ (x ...) e)
     number
     +)
  (E (v ... E e ...)
     (if0 E e e)
     hole)
  (x (variable-except λ + if0)))

(try-it 100 λv E)
