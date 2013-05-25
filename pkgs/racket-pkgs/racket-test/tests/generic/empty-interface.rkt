#lang racket
(require racket/generic)
(define-generics name)
(struct foo ()
  #:methods gen:name [])
