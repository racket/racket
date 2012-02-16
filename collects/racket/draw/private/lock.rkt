#lang racket/base
(require ffi/unsafe/atomic)
(provide atomically)
(define-syntax-rule 
  (atomically e1 e2 ...)
  (begin (start-atomic)
         (begin0 (let () e1 e2 ...)
                 (end-atomic))))

