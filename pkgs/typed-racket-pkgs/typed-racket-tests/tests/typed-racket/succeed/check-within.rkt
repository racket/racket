
#lang typed-scheme

(require scheme/math typed/test-engine/scheme-tests)

(define-struct: circle ({radius : Number}))
(: circle-area (circle -> Number))

(check-within (+ 1 2.14) pi .1)
(check-range 2 1 3)
(check-member-of 'a 'b 'c 'd 'a 'z)
(check-error (error "fail") "fail")

(define (circle-area c)
 (* pi (circle-radius c) (circle-radius c)))

(test)
