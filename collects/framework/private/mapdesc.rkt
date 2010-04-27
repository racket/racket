#lang scheme/base

(provide mapdesc)
(require (for-syntax scheme/base)
         scribble/decode
         scribble/manual)

(define-syntax (mapdesc stx)
  (syntax-case stx ()
    [(_ cmd events)
     #'(make-splice (list (index (symbol->string 'cmd))
                          (symbol->string 'cmd)
                          " ("
                          (symbol->string 'events)
                          " events)"))]))
