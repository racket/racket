#lang racket/base

(require "xml.rkt" "html.rkt" "resource.rkt"
         ;; includes all of the scribble/text utilities
         scribble/text)

(provide (all-from-out "xml.rkt" "html.rkt" "resource.rkt" scribble/text)
         (rename-out [top #%top]))

(require (for-syntax racket/base))
(define-syntax (top stx)
  (syntax-case stx ()
    [(_ . x)
     (let ([x* (syntax-e #'x)])
       (if (and (symbol? x*) (regexp-match? #rx":$" (symbol->string x*)))
         #''x
         #'(#%top . x)))]))
