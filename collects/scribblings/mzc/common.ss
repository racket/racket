#lang scheme/base
(require scribble/manual)

(provide mzc
         inside-doc)

(define mzc (exec "mzc"))

(define inside-doc
  '(lib "scribblings/inside/inside.scrbl"))
