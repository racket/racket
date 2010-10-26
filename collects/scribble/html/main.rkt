#lang racket/base

(provide (except-out (all-from-out racket/base) #%top #%module-begin)
         (rename-out [top #%top] [module-begin #%module-begin])
         ;; to be used as a text language (output via `output-xml')
         (all-from-out scribble/text)
         ;; provide a `text' alias and an `include' alias
         (rename-out [begin/text text] [include/text include])
         ;; main functionality
         (all-from-out "xml.rkt" "html.rkt" "resource.rkt"
                       racket/list racket/string))

(require "xml.rkt" "html.rkt" "resource.rkt" racket/list racket/string
         scribble/text scribble/text/syntax-utils (for-syntax racket/base))

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
