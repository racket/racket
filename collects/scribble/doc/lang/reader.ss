#lang s-exp syntax/module-reader

scribble/doclang

#:read        scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t
#:wrapper1 (lambda (t) (list* 'doc 'values '() (t)))
#:module-info (scribble-base-module-info)
#:info (scribble-base-info)

(require (prefix-in scribble: "../../reader.ss")
         scribble/base/reader)
