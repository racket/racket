#lang typed/racket

(struct: x ())
(define-struct: y ())

(x) (y)

(provide (struct-out x) (struct-out y))
