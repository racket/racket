#lang s-exp syntax/module-reader

typed-scheme

#:read r:read
#:read-syntax r:read-syntax
#:info make-info

(require (prefix-in r: typed-racket/typed-reader))

(define (make-info key default use-default)
  (case key
    [else (use-default key default)]))
