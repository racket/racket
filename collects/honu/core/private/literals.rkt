#lang racket/base

(provide (all-defined-out))
(require syntax/parse
         (for-syntax racket/base
                     syntax/parse))

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
                honu-<-
                honu-literal
                honu-then
                honu-? honu-: honu-comma honu-. #%braces #%brackets #%parens colon
                honu-and
                ellipses-comma ellipses-comma* ellipses-repeat
                honu-in
                honu-for-syntax
                honu-for-template
                honu-prefix
                %racket
                %racket-expression)

(define-syntax-rule (define-literal+set set literal ...)
                    (begin
                      (define-literal literal ...)
                      (begin-for-syntax
                        (define-literal-set set (literal ...)))))

(define-literal-set cruft (#%parens #%brackets #%braces
                           %racket %racket-expression
                           semicolon colon honu-comma honu-<-))
