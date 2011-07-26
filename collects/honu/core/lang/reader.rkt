#lang s-exp syntax/module-reader

honu/core/main

#:read honu-read 
#:read-syntax honu-read-syntax
#:whole-body-readers? #t

(require "../read.rkt")
