#lang s-exp syntax/module-reader

honu

#:read honu-read
#:read-syntax honu-read-syntax
#:whole-body-readers? #t
#:info honu-info

(require "../core/read.rkt"
         "../core/language.rkt")
