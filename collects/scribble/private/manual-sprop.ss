#lang scheme/base

(require "../html-properties.ss"
         "../latex-properties.ss"
         "on-demand.ss"
         setup/main-collects
         scheme/promise)
           
(provide scheme-properties)

(define-on-demand scheme-properties
  (let ([abs (lambda (s)
               (path->main-collects-relative (build-path (collection-path "scribble") s)))])
    (list (make-css-addition (abs "scheme.css"))
          (make-tex-addition (abs "scheme.tex")))))
