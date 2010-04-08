#lang s-exp scribble/base/reader
scribble/doclang
#:wrapper1 (lambda (t) (list* 'doc 'values '() (t)))
