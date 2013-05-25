#lang racket/base
(require (only-in racket/gui/dynamic
                  gui-available?))

(provide gui-available?
         gui-dynamic-require)

(define (gui-dynamic-require sym)
  (parameterize ([current-namespace (variable-reference->empty-namespace
                                     (#%variable-reference))])
    (if (gui-available?)
        (dynamic-require 'mred sym)
        (error "scheme/gui/base is not available"))))
