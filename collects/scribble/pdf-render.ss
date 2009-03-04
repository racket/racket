#lang scheme/base

(require "private/indirect-renderer.ss" "private/run-pdflatex.ss"
         (prefix-in latex: "latex-render.ss"))

(provide render-mixin)

(define render-mixin
  (make-indirect-renderer-mixin
   latex:render-mixin #".tex" #".pdf" run-pdflatex))
