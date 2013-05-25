#lang s-exp syntax/module-reader

typed-racket/minimal

#:language-info make-language-info
#:info make-info

(define (make-info key default use-default)
  (case key
    [else (use-default key default)]))

(define make-language-info
  `#(typed-racket/language-info get-info ()))
