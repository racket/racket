#lang racket/base

(define (default-reader-guard v) v)

(provide current-reader-guard)
(define current-reader-guard
  (make-parameter default-reader-guard
                  (lambda (v)
                    (unless (and (procedure? v)
                                 (procedure-arity-includes? v 1))
                      (raise-argument-error 'current-reader-guard
                                            "(procedure-arity-includes/c 1)"
                                            v))
                    v)))

(define-syntax-rule (define-boolean-parameter id val)
  (begin
    (provide id)
    (define id (make-parameter val (lambda (v) (and v #t))))))

;; (define-boolean-parameter read-case-sensitive #t) - shared with printer
(define-boolean-parameter read-square-bracket-as-paren #t)
(define-boolean-parameter read-curly-brace-as-paren #t)
(define-boolean-parameter read-square-bracket-with-tag #f)
(define-boolean-parameter read-curly-brace-with-tag #f)
(define-boolean-parameter read-cdot #f)
(define-boolean-parameter read-accept-graph #t)
(define-boolean-parameter read-accept-compiled #f)
(define-boolean-parameter read-accept-box #t)
;; (define-boolean-parameter read-accept-bar-quote #t) - shared with printer
(define-boolean-parameter read-decimal-as-inexact #t)
(define-boolean-parameter read-accept-dot #t)
(define-boolean-parameter read-accept-infix-dot #t)
(define-boolean-parameter read-accept-quasiquote #t)
(define-boolean-parameter read-accept-reader #f)
(define-boolean-parameter read-accept-lang #t)
