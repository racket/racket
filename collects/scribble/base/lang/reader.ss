#lang s-exp syntax/module-reader

scribble/base/lang

#:read        scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t
#:wrapper1 (lambda (t) (list* 'doc 'values '() (t)))

(require (prefix-in scribble: "../../reader.ss"))
