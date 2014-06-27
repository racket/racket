#lang scheme/base

(require "private/indirect-renderer.rkt" "private/run-pdflatex.rkt"
         (prefix-in latex: "latex-render.rkt"))

(provide render-mixin
         dvi-render-mixin)

(define render-mixin
  (make-indirect-renderer-mixin
   latex:render-mixin #".tex" #".pdf" 
   run-pdflatex))

(define dvi-render-mixin
  (make-indirect-renderer-mixin
   (λ (%) (latex:render-mixin % #:convert-as-ps-not-pdf? #t)) #".tex" #".pdf" 
   run-dvipdf-latex))
