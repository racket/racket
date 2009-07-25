#lang s-exp syntax/module-reader

scribble/manual/lang

#:read        scribble:read-inside
#:read-syntax scribble:read-syntax-inside
#:whole-body-readers? #t
#:wrapper1 (lambda (t) (cons 'doc (t)))

(require (prefix-in scribble: "../../reader.ss"))
