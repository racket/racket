#lang racket/base

(require (for-syntax racket/base)
         typed/untyped-utils
         (prefix-in typed: "typed-matrix-arithmetic.rkt")
         (rename-in "untyped-matrix-arithmetic.rkt"
                    [matrix-map  untyped:matrix-map]))

(define-typed/untyped-identifier matrix-map
  typed:matrix-map untyped:matrix-map)

(define-syntax (define/inline-macro stx)
  (syntax-case stx ()
    [(_ name pat inline-fun typed:fun)
     (syntax/loc stx
       (define-syntax (name inner-stx)
         (syntax-case inner-stx ()
           [(_ . pat)  (syntax/loc inner-stx (inline-fun . pat))]
           [(_ . es)   (syntax/loc inner-stx (typed:fun . es))]
           [_          (syntax/loc inner-stx typed:fun)])))]))

(define/inline-macro matrix* (a as ...) inline-matrix* typed:matrix*)
(define/inline-macro matrix+ (a as ...) inline-matrix+ typed:matrix+)
(define/inline-macro matrix- (a as ...) inline-matrix- typed:matrix-)
(define/inline-macro matrix-scale (a x) inline-matrix-scale typed:matrix-scale)

(define/inline-macro do-matrix-map (f a as ...) inline-matrix-map matrix-map)

(provide
 (rename-out [do-matrix-map  matrix-map]
             [typed:matrix=  matrix=]
             [typed:matrix-sum  matrix-sum])
 matrix*
 matrix+
 matrix-
 matrix-scale)
