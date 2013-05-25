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
                                             "this is a literal and cannot be used outside a macro" (syntax->datum stx))))
   ...))

(define-literal honu-return)
(define-literal semicolon)
(define-literal honu-|| honu-%
                honu-%=
                honu-&= honu-^= honu-\|= honu-<<= honu->>= honu->>>=
                honu->> honu-<< honu->>> 
                honu-!=
                ;; honu-equal
                honu-<-
                honu-literal
                honu-then
                honu-? honu-: honu-comma honu-. #%braces #%brackets #%parens colon
                ellipses-comma ellipses-comma* ellipses-repeat
                honu-in
                honu-where
                ;; honu-for-syntax
                honu-for-template
                honu-prefix
                honu-$
                ;; FIXME: in-lines should probably not be here
                honu-in-lines
                postfix)

(define-syntax-rule (define-literal+set set literal ...)
                    (begin
                      (define-literal literal ...)
                      (begin-for-syntax
                        (define-literal-set set (literal ...)))))

(define-literal-set cruft (#%parens #%brackets #%braces
                           semicolon colon honu-comma))
