#lang s-exp syntax/module-reader

scribble/lp/lang/lang

#:read read-inside
#:read-syntax read-syntax-inside
#:whole-body-readers? #t
#:info (scribble-base-info)
#:module-info (scribble-base-module-info)

(require scribble/reader
         (only-in scribble/base/reader
                  scribble-base-info
                  scribble-base-module-info))


