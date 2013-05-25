#lang scheme/base

(provide stdin stdout stderr cd)
(define stdin  current-input-port)
(define stdout current-output-port)
(define stderr current-error-port)
(define cd     current-directory)

(provide current-file)
(define current-file (make-parameter #f))

(provide add-eval do-evals)
(define evals (make-parameter '()))
(define (add-eval expr) (evals (cons expr (evals))))
(define (do-evals) (for-each eval (reverse (evals))))
