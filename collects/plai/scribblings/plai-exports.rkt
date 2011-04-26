#lang racket/base
(require (for-label (except-in plai #%module-begin provide))
         scribble/manual)
(provide plai:print-only-errors
         plai:halt-on-errors)
(define plai:print-only-errors (racket print-only-errors))
(define plai:halt-on-errors (racket halt-on-errors))

