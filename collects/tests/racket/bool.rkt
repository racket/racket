#lang racket/base
(require racket/bool rackunit)

(check-true true)
(check-false false)

(check-true (boolean=? #t #t))
(check-false (boolean=? #t #f))
(check-exn #rx"^boolean=?" (λ () (boolean=? #f 11)))
(check-exn #rx"^boolean=?" (λ () (boolean=? 11 #f)))

(check-true (symbol=? 'x 'x))
(check-false (symbol=? 'x 'y))
(check-exn #rx"^symbol=?" (λ () (symbol=? 'z 11)))
(check-exn #rx"^symbol=?" (λ () (symbol=? 11 'z)))

(check-true (false? #f))
(check-false (false? #t))
(check-false (false? "11"))

