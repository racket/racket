#lang scheme/base
(require scribble/manual)

(provide inside-doc
         reference-doc)

(define inside-doc
  '(lib "scribblings/inside/inside.scrbl"))

(define guide-doc
  '(lib "scribblings/guide/guide.scrbl"))

(define reference-doc
  '(lib "scribblings/reference/reference.scrbl"))
