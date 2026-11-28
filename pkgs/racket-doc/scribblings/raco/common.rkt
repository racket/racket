#lang scheme/base
(require scribble/manual)

(provide inside-doc
         guide-doc
         reference-doc

         CS
         BC

         cross-phase-persistent-lib)

(define inside-doc
  '(lib "scribblings/inside/inside.scrbl"))

(define guide-doc
  '(lib "scribblings/guide/guide.scrbl"))

(define reference-doc
  '(lib "scribblings/reference/reference.scrbl"))

(define CS (tech #:doc guide-doc "CS"))
(define BC (tech #:doc guide-doc "BC"))

;; For use in a `defmodule` body: @cross-phase-persistent-lib[setup/dirs]
(define-syntax-rule (cross-phase-persistent-lib lib)
  (cross-phase-persistent-lib-note (racketmodname lib)))

(define (cross-phase-persistent-lib-note lib-elem)
  (list "The " lib-elem " library is a "
        (tech #:doc reference-doc "cross-phase persistent")
        " module."))
