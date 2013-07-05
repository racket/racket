#lang racket/base
(require racket/runtime-path)
;; this uses a dynamic require here so that
;; the zo compiler does not see a connection 
;; between this file and typesetting-and-macros.scrbl
;; so that we can avoid compiling that file. 
(define-runtime-path typesetting-and-macros.scrbl "typesetting-and-macros.scrbl")
(define doc (dynamic-require typesetting-and-macros.scrbl 'doc))
(provide doc)
