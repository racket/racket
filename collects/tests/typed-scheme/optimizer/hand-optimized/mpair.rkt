#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(: x (MPairof Integer Float))
(define x (mcons 1 1.0))
(unsafe-mcar x)
(unsafe-mcdr x)
(unsafe-set-mcar! x (+ 1 2))
(unsafe-set-mcdr! x (+ 1.0 2.0))

(: f ((MListof Integer) -> Integer))
(define (f x)
  (if (null? x)
      0
      (unsafe-mcar x)))
