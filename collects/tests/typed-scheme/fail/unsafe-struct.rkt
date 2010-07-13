#;
(exn-pred 3)
#lang typed/scheme
(require racket/unsafe/ops)

(define-struct: x ((a : Integer) (b : String)) #:mutable)

(define x1 (make-x 1 "1"))

(+ (unsafe-struct-ref x1 1) 1)

(unsafe-struct-set! x1 0 "2")
(unsafe-struct-set! x1 1 1)
