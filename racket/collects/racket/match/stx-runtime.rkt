#lang racket/base

(require racket/stxparam
         (for-syntax racket/base))

(provide fail hash-pattern-optimized?)

(define-syntax-parameter fail
  (lambda (stx)
    (raise-syntax-error
     #f "used out of context: not in match pattern" stx)))

;; If true, optimize the hash pattern as follows:
;; - Generate simplified code for the open mode
;; - Treat #:rest _ as the open mode
(define-syntax-parameter hash-pattern-optimized? #t)
