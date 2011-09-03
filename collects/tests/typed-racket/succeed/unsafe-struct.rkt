#lang typed/scheme
(require racket/unsafe/ops)

(define-struct: x ((a : Integer) (b : String)) #:mutable)

(define x1 (make-x 1 "1"))

(= (+ (unsafe-struct-ref x1 0) 2) 3)
(string=? (string-append (unsafe-struct-ref x1 1) "\n") "1\n")

(unsafe-struct-set! x1 0 2)
(unsafe-struct-set! x1 1 "2")
(= (+ (unsafe-struct-ref x1 0) 2) 4)
(string=? (string-append (unsafe-struct-ref x1 1) "\n") "2\n")
