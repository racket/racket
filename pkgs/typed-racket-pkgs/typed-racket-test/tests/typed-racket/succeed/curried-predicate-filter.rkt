#lang typed/racket

;; This test ensures that curried predicates have
;; the correct filters so that they can be used for
;; occurrence typing.

(define f (λ (x) (λ (y) (number? x))))

(: b (U Number String))
(define b 5)

(define g (f b))

;; this doesn't type-check unless OT is working
(if (g "foo") (add1 b) 3)

;; The following ensures that the correct filter can be
;; written by the user
(let ()
  (: f (Any -> (Any -> Boolean : #:+ (Number @ 1 0)
                                 #:- (! Number @ 1 0))
            : #:+ Top #:- Bot))
  (define f (λ (x) (λ (y) (number? x))))
  (: b (U Number String))
  (define b 5)
  (define g (f b))
  (if (g "foo") (add1 b) 3))

