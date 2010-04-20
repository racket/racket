#lang racket/base

(require racket/stxparam
         (for-syntax racket/base))

(provide match-equality-test
         exn:misc:match?
         match:error
         fail
         matchable?
         match-prompt-tag)

(define match-prompt-tag (make-continuation-prompt-tag 'match))

(define match-equality-test (make-parameter equal?))

(define-struct (exn:misc:match exn:fail) (value))

(define (match:error val)
  (raise (make-exn:misc:match (format "match: no matching clause for ~e" val)
                              (current-continuation-marks)
                              val)))

(define-syntax-parameter fail
  (lambda (stx)
    (raise-syntax-error
     #f "used out of context: not in match pattern" stx)))

;; can we pass this value to regexp-match?
(define (matchable? e)
  (or (string? e) (bytes? e)))
