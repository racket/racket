#lang s-exp syntax/module-reader

typed-scheme/minimal

#:language-info make-language-info
#:info make-info

(define (make-info key default use-default)
  (case key
    [else (use-default key default)]))

(define make-language-info
  `#(typed-scheme/language-info get-info ()))
