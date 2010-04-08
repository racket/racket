#lang scheme/base

(provide scribble-base-info
         scribble-base-module-info)

(define (scribble-base-info)
  (lambda (key defval default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/scribble-lexer 'scribble-inside-lexer)]
      [(drscheme:toolbar-buttons)
       (dynamic-require 'scribble/tools/drscheme-buttons 'drscheme-buttons)]
      [else (default key defval)])))

(define (scribble-base-module-info)
  (lambda (modpath data)
    #f))
