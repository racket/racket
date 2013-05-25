#lang racket/base

(require "main.rkt" (except-in scribble/text/lang #%top)
         scribble/text/syntax-utils)

(provide (except-out (all-from-out scribble/text/lang) #%module-begin)
         (rename-out [module-begin #%module-begin])
         (all-from-out "main.rkt"))

(require (for-syntax racket/base))
(define-syntax-rule (module-begin expr ...)
  (#%plain-module-begin
   (port-count-lines! (current-output-port))
   (process-begin/text begin output-xml expr ...)))
