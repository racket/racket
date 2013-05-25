#lang s-exp syntax/module-reader

honu/core/main

#:read honu-read 
#:read-syntax honu-read-syntax
#:whole-body-readers? #t
#:info honu-info

(require "../read.rkt")
(require "../language.rkt")
