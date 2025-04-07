#lang racket/base
(require "avoid-leaf.rkt") ; encourage demod of this module

(provide module-begin)

(define-syntax-rule (module-begin)
  (#%module-begin "ok"))
