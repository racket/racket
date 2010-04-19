#lang s-exp syntax/module-reader

scribble/text/textlang

#:read        scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t
#:info        (scribble-base-reader-info)

(require (prefix-in scribble: "../../reader.ss")
         (only-in scribble/base/reader
                  scribble-base-reader-info))
