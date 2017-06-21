#lang racket/base
(require (for-syntax racket/base))

;; == Keywords

(define-for-syntax (bad-keyword-use stx)
  (raise-syntax-error #f "keyword used out of context" stx))

(define-syntax-rule (define-keyword name)
  (begin
    (provide name)
    (define-syntax name bad-keyword-use)))

(define-keyword pattern)
(define-keyword ~var)
(define-keyword ~datum)
(define-keyword ~literal)
(define-keyword ~and)
(define-keyword ~or)
(define-keyword ~not)
(define-keyword ~seq)
(define-keyword ~between)
(define-keyword ~once)
(define-keyword ~optional)
(define-keyword ~rest)
(define-keyword ~describe)
(define-keyword ~!)
(define-keyword ~bind)
(define-keyword ~fail)
(define-keyword ~parse)
(define-keyword ~do)
(define-keyword ...+)
(define-keyword ~delimit-cut)
(define-keyword ~commit)
(define-keyword ~reflect)
(define-keyword ~splicing-reflect)
(define-keyword ~post)
(define-keyword ~eh-var)
(define-keyword ~peek)
(define-keyword ~peek-not)

(define-keyword ~or*)
(define-keyword ~alt)
