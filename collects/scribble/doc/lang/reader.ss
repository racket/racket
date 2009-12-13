#lang scheme/base
(require (prefix-in doc: scribble/doc/reader))
(provide (rename-out [doc:read read] [doc:read-syntax read-syntax])
         get-info)

(define (get-info . args)
  (lambda (key defval)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/scribble-lexer 'scribble-inside-lexer)]
      [(drscheme:toolbar-buttons)
       (dynamic-require 'scribble/tools/drscheme-buttons 'drscheme-buttons)]
      [else defval])))
