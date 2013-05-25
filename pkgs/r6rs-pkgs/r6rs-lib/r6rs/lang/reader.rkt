#lang s-exp syntax/module-reader

r6rs

#:wrapper1 with-r6rs-reader-parameters
#:language-info '#(scheme/language-info get-info #f)

(require "../private/readtable.rkt")
