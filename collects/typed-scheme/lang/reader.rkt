#lang s-exp syntax/module-reader

typed-scheme

#:read r:read
#:read-syntax r:read-syntax
#:info make-info

(require (prefix-in r: typed-racket/typed-reader))

(define (make-info key default use-default)
  (case key
    [(drscheme:toolbar-buttons)
     (list (dynamic-require 'typed-racket/optimizer/tool/tool
                            'optimization-coach-drracket-button))]
    [else (use-default key default)]))
