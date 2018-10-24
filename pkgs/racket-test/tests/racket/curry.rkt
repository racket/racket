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

(define (kw #:a a #:b b #:c c)
  (list a b c))

(check-equal? (((curry kw #:a 1) #:b 2) #:c 3) (list 1 2 3))
(check-equal? (((curry kw #:b 1) #:c 2) #:a 3) (list 3 1 2))
(check-equal? (((curry kw #:c 1) #:a 2) #:b 3) (list 2 3 1))
(check-equal? (((curry kw) #:a 1 #:b 2) #:c 3) (list 1 2 3))
(check-equal? (((curry kw) #:b 1 #:c 2) #:a 3) (list 3 1 2))
(check-equal? (((curry kw) #:c 1 #:a 2) #:b 3) (list 2 3 1))

(check-exn exn:fail:contract? (λ () (curry kw #:d 1)))
(check-exn exn:fail:contract? (λ () ((curry kw) #:d 1)))
(check-exn exn:fail:contract? (λ () (curry kw 1)))
(check-exn exn:fail:contract? (λ () ((curry kw) 1)))
(check-exn exn:fail:contract? (λ () (((curry kw) #:a 1) #:a 2)))
(check-exn exn:fail:contract? (λ () ((curry kw #:a 1) #:a 2)))

(define (kw+pos a b #:x x #:y y)
  (list a b x y))

(check-equal? ((((curry kw+pos 1) 2) #:x 3) #:y 4) (list 1 2 3 4))
(check-equal? ((((curry kw+pos 1) 2) #:y 3) #:x 4) (list 1 2 4 3))
(check-equal? ((curry kw+pos 1 #:x 3) 2 #:y 4) (list 1 2 3 4))
(check-equal? ((curry kw+pos 1 #:y 3) 2 #:x 4) (list 1 2 4 3))
(check-equal? ((curry kw+pos 1 #:x 3 #:y 4) 2) (list 1 2 3 4))
(check-equal? ((curry kw+pos 1 #:y 3 #:x 4) 2) (list 1 2 4 3))

(check-exn exn:fail:contract? (λ () ((curry kw+pos) 1 2 3)))
(check-exn exn:fail:contract? (λ () ((curry kw+pos) #:d 1)))

(define (opt-kw+pos #:a [a #f] b)
  (list a b))

(check-pred procedure? (curry opt-kw+pos #t))
(check-pred procedure? ((curry opt-kw+pos) #t))
(check-equal? (curry opt-kw+pos #:a #t #t) (list #t #t))
(check-equal? ((curry opt-kw+pos) #:a #t #t) (list #t #t))

(define zero-or-one
  (case-lambda
    [() 0]
    [(a) 1]))

(check-equal? (((curry zero-or-one))) 0)
(check-equal? ((curry zero-or-one) #t) 1)
(check-equal? (curry zero-or-one #t) 1)

(define (zero-or-one/kw [x #f] #:y y)
  (if x 1 0))

(check-equal? ((curry zero-or-one/kw #:y #f)) 0)
(check-equal? ((curry zero-or-one/kw #:y #f) #t) 1)
(check-equal? (curry zero-or-one/kw #:y #f #t) 1)
