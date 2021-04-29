#lang scheme/base
(require scribble/manual)

(provide inside-doc
         guide-doc
         reference-doc

         CS
         BC)

(define inside-doc
  '(lib "scribblings/inside/inside.scrbl"))

(define guide-doc
  '(lib "scribblings/guide/guide.scrbl"))

(define reference-doc
  '(lib "scribblings/reference/reference.scrbl"))

(define CS (tech #:doc guide-doc "CS"))
(define BC (tech #:doc guide-doc "BC"))
