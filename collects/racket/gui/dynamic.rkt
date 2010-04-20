#lang scheme/base

(provide gui-available?
         gui-dynamic-require)

(define (gui-available?)
  (and (zero? (variable-reference->phase (#%variable-reference)))
       (with-handlers ([exn:fail? (lambda (exn) #f)])
         (eq? (dynamic-require 'mred/private/dynamic 'kernel-initialized)
              'done))))

(define-namespace-anchor anchor)

(define (gui-dynamic-require sym)
  (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
    (dynamic-require 'mred sym)))
