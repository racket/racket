#lang racket/base
(require racket/serialize)

(provide (all-defined-out))

(define-serializable-struct cls/intf
  (name-element app-mixins super intfs methods)
  #:transparent)
