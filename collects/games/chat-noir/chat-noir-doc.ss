#lang scribble/doc

@(begin

(require (for-syntax scheme/base
                     syntax/boundmap
                     scheme/list
                     compiler/cm-accomplice)
         scribble/manual
         scribble/struct
         scribble/basic
         scribble/decode
         scheme/include)

;; define `chunk' as a macro that typesets the code
(define-syntax (chunk stx)
  (syntax-case stx ()
    [(_ name expr ...)
     (cond
       [(not (identifier? #'name))
        (raise-syntax-error #f "expected a chunk name" stx #'name)]
       [(not (regexp-match? #rx"^<.*>$" (symbol->string (syntax-e #'name))))
        (raise-syntax-error
         #f "chunk names must begin and end with angle brackets, <...>"
         stx #'name)]
       [else #`(make-splice (list (emph (scheme name) " ::=")
                                  (schemeblock expr ...)))])]))

(define-syntax module
  (syntax-rules () [(module name base body ...) (begin body ...)]))
(include "chat-noir-literate.ss")

)
