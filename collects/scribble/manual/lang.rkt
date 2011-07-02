#lang scheme/base
(require scribble/doclang 
         scribble/manual
         "../private/defaults.rkt")
(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/manual)
         (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin id . body)
  (#%module-begin id post-process () . body))

(define (post-process doc)
  (add-defaults doc
                (scribble-file "manual-prefix.tex")
                (scribble-file "manual-style.tex")
                null
                #t))
