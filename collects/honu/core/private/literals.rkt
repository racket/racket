#lang racket

(provide (all-defined-out))
(require syntax/parse)

;; macro for defining literal tokens that can be used in macros
(define-syntax-rule (define-literal name ...)
  (begin
   (define-syntax name (lambda (stx)
                         (raise-syntax-error 'name
                                             "this is a literal and cannot be used outside a macro")))
   ...))

(define-literal honu-return)
(define-literal semicolon)
(define-literal honu-+ honu-* honu-/ honu-- honu-|| honu-%
                honu-= honu-+= honu--= honu-*= honu-/= honu-%=
                honu-&= honu-^= honu-\|= honu-<<= honu->>= honu->>>=
                honu->> honu-<< honu->>> honu-< honu-> honu-<= honu->=
                honu-!= honu-==
                honu-literal
                honu-? honu-: honu-comma honu-. #%braces #%brackets #%parens colon
                honu-and
                ellipses-comma ellipses-comma* ellipses-repeat
                honu-for-syntax
                honu-for-template)

(define-literal-set cruft (#%parens #%brackets #%braces semicolon))
