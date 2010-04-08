#lang s-exp scribble/base/reader
scribble/base/lang
#:wrapper1 (lambda (t) (list* 'doc 'values '() (t)))
