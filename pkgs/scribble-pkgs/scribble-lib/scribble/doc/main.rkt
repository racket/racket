#lang scheme/base
(define-syntax-rule (out)
  (begin (require scribble/doclang)
         (provide (all-from-out scribble/doclang))))
(out)
