#lang racket/base

(provide
 template
 template?
 template-strings
 template-interpolations
 interpolation
 interpolation?
 interpolation-value
 interpolation-syntax
 interpolation-kind
 interpolation-source
) ; end provide

(struct template (strings interpolations) #:transparent)
(struct interpolation (value syntax kind source) #:transparent)
