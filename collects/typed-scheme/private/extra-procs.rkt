#lang scheme/base
(provide assert)

(define-syntax assert
  (syntax-rules ()
    ((assert v)
     (or v (error "Assertion failed")))
    ((assert v pred)
     (let ((val v))
       (if (pred val) val (error "Assertion failed"))))))
