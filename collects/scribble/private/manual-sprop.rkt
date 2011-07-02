#lang scheme/base

(require "../html-properties.rkt"
         "../latex-properties.rkt"
         "on-demand.rkt"
         setup/main-collects
         scheme/promise)

(provide scheme-properties)

(define-on-demand scheme-properties
  (let ([abs (lambda (s)
               (path->main-collects-relative (collection-file-path s "scribble")))])
    (list (make-css-addition (abs "racket.css"))
          (make-tex-addition (abs "racket.tex")))))
