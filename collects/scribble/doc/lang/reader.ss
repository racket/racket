#lang scheme/base
(require (prefix-in doc: scribble/doc/reader))
(provide (rename-out [doc:read read] 
                     [my:read-syntax read-syntax])
         get-info)

(define (my:read-syntax . args)
  (let ([s (apply doc:read-syntax args)])
    ;; For now, remove the 'module-language property added by `doc:read-syntax'
    (syntax-property s 'module-language #f)))

(define (get-info . args)
  (lambda (key defval)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/scribble-lexer 'scribble-inside-lexer)]
      [(drscheme:toolbar-buttons)
       (dynamic-require 'scribble/tools/drscheme-buttons 'drscheme-buttons)]
      [else defval])))
