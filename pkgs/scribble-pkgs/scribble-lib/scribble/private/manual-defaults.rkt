#lang scheme/base
(require scribble/core
         scribble/html-properties
         "defaults.rkt"
         "on-demand.rkt")

(provide post-process
         manual-doc-style)

(define (post-process doc)
  (add-defaults doc
                (scribble-file "manual-prefix.tex")
                (scribble-file "manual-style.tex")
                null
                #:html (html-defaults (scribble-file "scribble-prefix.html")
                                      (scribble-file "manual-style.css")
                                      (list
                                       (scribble-file "manual-fonts.css")))
                #:properties (list
                              (css-style-addition
                               (scribble-file "manual-racket.css"))
                              (js-style-addition
                               (scribble-file "manual-racket.js")))
                #t))

(define-on-demand manual-doc-style
  (part-style (post-process (part #f null #f plain null null null))))
