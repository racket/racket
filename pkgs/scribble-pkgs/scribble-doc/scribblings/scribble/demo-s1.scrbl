#lang racket/base
(require "demo.scrbl"
         scribble/core)

(define renamed-doc
  (struct-copy part doc
               [title-content
                (cons "S1 " (part-title-content doc))]))

(provide (rename-out [renamed-doc doc]))
