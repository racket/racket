#lang racket/base
(require "demo-manual.scrbl"
         scribble/core)

(define renamed-doc
  (struct-copy part doc
               [title-content
                (cons "M1 " (part-title-content doc))]))

(provide (rename-out [renamed-doc doc]))
