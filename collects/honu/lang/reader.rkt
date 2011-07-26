#lang s-exp syntax/module-reader

honu

#:read honu-read
#:read-syntax honu-read-syntax
#:whole-body-readers? #t

(require "../core/read.rkt")
