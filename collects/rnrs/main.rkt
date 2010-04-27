#lang scheme/base

(define-syntax-rule (bounce)
  (begin
    (require rnrs/main-6)
    (provide (all-from-out rnrs/main-6))))
(bounce)
