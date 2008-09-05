#lang s-exp syntax/module-reader

scribble/text/textlang

#:read        scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t

(require (prefix-in scribble: "../../reader.ss"))
