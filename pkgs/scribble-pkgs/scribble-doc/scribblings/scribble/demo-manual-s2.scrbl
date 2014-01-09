#lang racket/base
(require "demo-manual.scrbl"
         scribble/core
         scribble/manual)

(define renamed-doc
  (struct-copy part doc
               [style manual-doc-style]
               [title-content
                (cons "S2 " (part-title-content doc))]))

(provide (rename-out [renamed-doc doc]))
