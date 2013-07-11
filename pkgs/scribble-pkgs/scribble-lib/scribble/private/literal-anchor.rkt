#lang racket/base
(require racket/serialize)

(provide (all-defined-out))

(define-serializable-struct literal-anchor (string)
  #:transparent)
