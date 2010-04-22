#lang s-exp syntax/module-reader

scribble/lp/lang/lang

#:read read-inside
#:read-syntax read-syntax-inside
#:whole-body-readers? #t
#:info (scribble-base-info)
#:language-info (scribble-base-language-info)

(require scribble/reader
         (only-in scribble/base/reader
                  scribble-base-info
                  scribble-base-language-info))


