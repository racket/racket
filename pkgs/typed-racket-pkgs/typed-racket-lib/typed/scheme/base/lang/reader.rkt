#lang s-exp syntax/module-reader

typed/scheme/base

#:read r:read
#:read-syntax r:read-syntax
#:info make-info
#:language-info make-language-info

(define (make-info key default use-default)
  (case key
    [(drscheme:toolbar-buttons)
     ;; If Optimization Coach is installed, load it.
     (with-handlers ([exn:fail:filesystem? (lambda _ '())]) ; not found
       (collection-path "optimization-coach")
       (list (dynamic-require 'optimization-coach/tool
                              'optimization-coach-drracket-button)))]
    [else (use-default key default)]))

(define make-language-info
  `#(typed-racket/language-info get-info ()))


(require (prefix-in r: typed-racket/typed-reader))
