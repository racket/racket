#lang racket/base

;; Tests for FFI unions

(require ffi/unsafe
         rackunit)

(define union-type (_union (_list-struct _int _int)))
(define union-type-2 (_union (_list-struct _int _int)
                             (_list-struct _double _double)))

(define val (cast (list 1 2) (_list-struct _int _int) union-type))
(define val-2 (cast (list 1.2 2.2) (_list-struct _double _double) union-type-2))

(check-equal? (car (union-ref val 0)) 1)
(check-equal? (car (union-ref val-2 1)) 1.2)

(union-set! val-2 0 (list 5 4))
(check-equal? (car (union-ref val-2 0)) 5)

(check-exn #rx"expected: list of c types" (λ () (_union 3)))
(check-exn #rx"expected: list of c types" (λ () (_union _int 4)))
(check-not-exn (λ () (_union _int _int)))

(check-exn #rx"too large" (λ () (union-ref val 1)))
(check-exn #rx"nonnegative-integer" (λ () (union-ref val -1)))
(check-exn #rx"nonnegative-integer" (λ () (union-ref val "foo")))
(check-exn #rx"too large" (λ () (union-ref val-2 2)))
(check-exn #rx"too large" (λ () (union-set! val 1 (list 1 2))))
(check-exn #rx"nonnegative-integer" (λ () (union-set! val -1 (list 1 2))))
(check-exn #rx"nonnegative-integer" (λ () (union-set! val "foo" (list 1 2))))
