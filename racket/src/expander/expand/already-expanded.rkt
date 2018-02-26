#lang racket/base

;; Defines a struct type for an expression that has been
;; expanded already by `local-expand-expression`

(provide (struct-out already-expanded))

(struct already-expanded (s binding-layer)
  #:reflection-name 'expanded-syntax)
