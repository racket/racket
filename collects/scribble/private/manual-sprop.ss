#lang scheme/base

(require "../html-properties.ss"
         "../latex-properties.ss"
         setup/main-collects)
           
(provide scheme-properties)

(define scheme-properties
  (let ([abs (lambda (s)
               (path->main-collects-relative (build-path (collection-path "scribble") s)))])
    (list (make-css-addition (abs "scheme.css"))
          (make-tex-addition (abs "scheme.tex")))))
