#lang racket/base

(provide (except-out (all-from-out racket/base) #%top)
         (rename-out [top #%top])
         ;; to be used as a text language
         (all-from-out scribble/text)
         ;; provide a `text' alias
         (rename-out [begin/text text])
         ;; main functionality
         (all-from-out "xml.rkt" "html.rkt" "resource.rkt"))

(require "xml.rkt" "html.rkt" "resource.rkt"
         scribble/text (for-syntax racket/base))

(define-syntax (top stx)
  (syntax-case stx ()
    [(_ . x)
     (let ([x* (syntax-e #'x)])
       (if (and (symbol? x*) (regexp-match? #rx":$" (symbol->string x*)))
         #''x
         #'(#%top . x)))]))
