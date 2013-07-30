#lang typed/racket

;; This test used to fail when the non-recursive type
;; aliases were registered *after* the recursive ones.
;;
;; Thanks to Havvy from IRC for the test

(define-type magic/int Integer)
(define-type magic/float Flonum)
(define-type magic/str String)
(define-type magic/symbol Symbol)
(define-type magic/map HashTable)
(define-type magic/invokable (magic/any * -> magic/any))
(define-type magic/any (U magic/int
                          magic/float
                          magic/str
                          magic/symbol
                          magic/map
                          magic/invokable))

(: x magic/any)
(define x "magic/str")

(: f magic/invokable)
(define f (lambda: (rst : magic/any *) (car rst)))

