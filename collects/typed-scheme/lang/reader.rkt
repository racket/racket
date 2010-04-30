#lang s-exp syntax/module-reader

typed-scheme

#:read r:read
#:read-syntax r:read-syntax

(require (prefix-in r: "../typed-reader.rkt"))
