#lang racket/base

(require "xml.rkt" "html.rkt" "resource.rkt"
         ;; includes all of the scribble/text utilities
         scribble/text)

(provide (all-from-out "xml.rkt" "html.rkt" "resource.rkt" scribble/text))
