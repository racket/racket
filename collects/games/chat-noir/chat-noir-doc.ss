#lang scribble/doc
@(require scribble/lp-include scheme/include)
;; HACK: use a fake `module', which makes it possible to include a module
;; and get only its code in.
@(define-syntax-rule (module name base body ...)
   (begin body ...))

@(include "chat-noir-literate.ss")
