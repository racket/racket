#lang racket/base
(require "embed-me37b.rkt")

(define form-list
  (dynamic-require ''basic:base 'list))

(form-list 'a
           (variable-reference->module-source
            (#%variable-reference)))
