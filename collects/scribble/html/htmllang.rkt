#lang racket/base

(require "main.rkt" scribble/text/textlang scribble/text/syntax-utils
         (for-syntax racket/base))

(provide (except-out (all-from-out scribble/text/textlang)
                     #%top #%module-begin)
         (rename-out [top #%top] [module-begin #%module-begin])
         (all-from-out "main.rkt"))

(define-syntax (top stx)
  (syntax-case stx ()
    [(_ . x)
     (let ([x* (syntax-e #'x)])
       (if (and (symbol? x*) (regexp-match? #rx":$" (symbol->string x*)))
         #''x
         #'(#%top . x)))]))

(define-syntax-rule (module-begin expr ...)
  (#%plain-module-begin
   (port-count-lines! (current-output-port))
   (process-begin/text begin output-xml expr ...)))
