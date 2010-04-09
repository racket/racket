#lang s-exp syntax/module-reader

typed/scheme

#:read r:read
#:read-syntax r:read-syntax
#:info make-info
#:module-info make-module-info

(define (make-info key default use-default)
  (case key
    [else (use-default key default)]))

(define make-module-info
  `#(typed-scheme/module-info module-info ()))


(require (prefix-in r: typed-scheme/typed-reader))
