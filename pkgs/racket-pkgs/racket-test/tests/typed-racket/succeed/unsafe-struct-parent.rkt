#lang typed/scheme
(require racket/unsafe/ops)

(define-struct: foo ((x : Integer) (y : String)))
(define-struct: (bar foo) ((z : Float)))

(define a (make-foo 1 "1"))
(define b (make-bar 2 "2" 2.0))

(= (+ (unsafe-struct-ref a 0) 2) 3)
(string=? (string-append (unsafe-struct-ref a 1) "\n") "1\n")
(= (+ (unsafe-struct-ref b 0) 2) 4)
(string=? (string-append (unsafe-struct-ref b 1) "\n") "2\n")
(= (+ (unsafe-struct-ref b 2) 2.0) 4.0)

(unsafe-struct-set! a 0 2)
(unsafe-struct-set! a 1 "2")
(unsafe-struct-set! b 0 3)
(unsafe-struct-set! b 1 "3")
(unsafe-struct-set! b 2 3.0)

(= (+ (unsafe-struct-ref a 0) 2) 4)
(string=? (string-append (unsafe-struct-ref a 1) "\n") "2\n")
(= (+ (unsafe-struct-ref b 0) 2) 5)
(string=? (string-append (unsafe-struct-ref b 1) "\n") "3\n")
(= (+ (unsafe-struct-ref b 2) 2.0) 5.0)
