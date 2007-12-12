#lang scheme/base

(provide gui-available?
         gui-dynamic-require)

(define (gui-available?)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (dynamic-require ''#%mred-kernel #f)
    #t))

(define-namespace-anchor anchor)

(define (gui-dynamic-require sym)
  (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
    (dynamic-require 'mred/mred sym)))
