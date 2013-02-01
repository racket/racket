#lang racket/base
(require scribble/doclang
         scribble/base
         "../private/defaults.rkt"
         scribble/latex-prefix)

(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin id . body)
  (#%module-begin id (post-process) () . body))

(define ((post-process) doc)
  (add-defaults doc
                (string->bytes/utf-8 (string-append "\\documentclass{book}\n"
                                                    unicode-encoding-packages))
                (scribble-file "book/style.tex")
                null
                #f))
