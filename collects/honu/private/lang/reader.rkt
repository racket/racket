#lang s-exp syntax/module-reader

honu/private/main

;;#:read honu-read
;;#:read-syntax honu-read-syntax
;;#:whole-body-readers? #t
#:language-info #(honu/core/language honu-language-info #f)
; #:language-info #(honu/core/runtime configure 'dont-care)

(require honu/core/read
         honu/core/language)
