#lang s-exp syntax/module-reader

typed/racket/base/no-check

#:read r:read
#:read-syntax r:read-syntax

(require (prefix-in r: typed-racket/typed-reader))
