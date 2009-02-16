#lang scribble/doc

@(begin

(require scribble/manual scribble/decode scheme/include)

;; define `chunk' as a macro that typesets the code
(define-syntax-rule (chunk name expr ...)
  (make-splice (list (emph (scheme name) " ::=")
                     (schemeblock expr ...))))

(define-syntax module
  (syntax-rules () [(module name base body ...) (begin body ...)]))
(include "chat-noir-literate.ss")

)
