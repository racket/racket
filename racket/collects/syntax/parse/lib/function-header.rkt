#lang racket/base

(require "../../parse.rkt"
         "../experimental/template.rkt")

(provide (all-defined-out))

(define-syntax-class function-header
  (pattern ((~or header:function-header name:id) . args:args)
           #:attr params
           (template ((?@ . (?? header.params ()))
                      . args.params))))

(define-syntax-class args
  (pattern (arg:arg ...)
           #:attr params #'(arg.name ...))
  (pattern (arg:arg ... . rest:id)
           #:attr params #'(arg.name ... rest)))

(define-splicing-syntax-class arg
  #:attributes (name)
  (pattern name:id)
  (pattern [name:id default])
  (pattern (~seq kw:keyword name:id))
  (pattern (~seq kw:keyword [name:id default])))

