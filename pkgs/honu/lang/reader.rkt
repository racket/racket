#lang s-exp syntax/module-reader

honu

#:read honu-read
#:read-syntax honu-read-syntax
#:whole-body-readers? #t
#:info honu-info
#:language-info #(honu/core/language honu-language-info #f)

(require "../core/read.rkt"
         "../core/language.rkt")
