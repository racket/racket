#lang at-exp scheme

(require scribble/manual scribble/core)
(provide (all-defined-out))

(define (item* header . args) (apply item @bold[header]{: } args))
(define-syntax-rule (tmod forms ...) (schememod typed-scheme forms ...))
(define (gtech . x)
  (apply tech x #:doc '(lib "scribblings/guide/guide.scrbl")))
(define (rtech . x)
  (apply tech x #:doc '(lib "scribblings/reference/reference.scrbl")))

(define ** (let ([* #f]) @scheme[*]))

(define-syntax-rule (annvar x t)
  (make-element #f (list @schemeparenfont["#{"]
			 @scheme[x : t]
			 @schemeparenfont["}"])))

(define-syntax-rule (annexpr x t)
  (make-element #f (list @schemeparenfont["#{"]
			 @scheme[x :: t]
			 @schemeparenfont["}"])))
