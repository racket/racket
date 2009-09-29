#!r6rs

(library (srfi :8)
  (export receive)
  (import (rnrs base))

(define-syntax receive
  (syntax-rules ()
    ((receive (var ...) ?producer . ?body)
     (let-values ([(var ...) ?producer]) . ?body))
    ((receive ?vars ?producer . ?body)
     (call-with-values (lambda () ?producer) (lambda ?vars . ?body))))))

