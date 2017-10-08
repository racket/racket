#lang racket/base
(require racket/function rackunit)

#| TODO: provide more comprehensive tests for
         other racket/function functions. |#

(check-pred procedure? (curry +))
(check-pred procedure? (curry + 1))
(check-equal? ((curry + 2) 3) 5)
(check-equal? ((curry + 2 3) 1) 6)

(define (three a b c)
  (+ a (* b c)))

(check-pred procedure? (curry three))
(check-pred procedure? (curry three 1))
(check-pred procedure? (curry three 1 2))
(check-pred procedure? ((curry three 1) 2))
(check-equal? ((curry three 1) 2 3) 7)
(check-equal? ((curry three 4 5) 6) 34)
(check-equal? (((curry three 3) 2) 1) 5)

(check-pred procedure? ((curry list) 1 2))
(check-pred procedure? ((curry cons) 1))

(check-equal? ((curry cons) 1 2) '(1 . 2))
(check-equal? (((curry list) 1 2) 3) '(1 2 3))
(check-equal? (((curry list) 1) 3) '(1 3))
(check-equal? ((((curry foldl) +) 0) '(1 2 3)) 6)

(check-exn exn:fail:contract? (λ () (curry 1 2)))
(check-exn exn:fail:contract? (λ () (curry 1)))
