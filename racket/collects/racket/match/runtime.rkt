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

(define-struct (exn:misc:match exn:fail) (value srclocs)
 #:property prop:exn:srclocs (lambda (ex) (exn:misc:match-srclocs ex)))


(define (match:error val srclocs form-name)
  (raise (make-exn:misc:match (format "~a: no matching clause for ~e" form-name val)
                              (current-continuation-marks)
                              val
                              srclocs)))

(define-syntax-parameter fail
  (lambda (stx)
    (raise-syntax-error
     #f "used out of context: not in match pattern" stx)))

;; can we pass this value to regexp-match?
(define (matchable? e)
  (or (string? e) (bytes? e)))
