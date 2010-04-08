#lang s-exp syntax/module-reader

scribble/lp/lang/lang

#:read read-inside
#:read-syntax read-syntax-inside
#:whole-body-readers? #t
#:module-info (scribble-base-module-info)
#:info (scribble-base-info)

(require scribble/reader
         scribble/base/reader)
