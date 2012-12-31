#lang racket/base

(module untyped-arithmetic-defs typed/racket/base
  (require "matrix-types.rkt"
           (prefix-in typed: "typed-matrix-arithmetic.rkt"))
  
  (provide (all-defined-out))
  
  (: matrix* ((Matrix Number) (Matrix Number) * -> (Matrix Number)))
  (define matrix* typed:matrix*)
  
  (: matrix+ ((Matrix Number) (Matrix Number) * -> (Matrix Number)))
  (define matrix+ typed:matrix+)
  
  (: matrix- ((Matrix Number) (Matrix Number) * -> (Matrix Number)))
  (define matrix- typed:matrix-)
  
  (: matrix-scale ((Matrix Number) Number -> (Matrix Number)))
  (define matrix-scale typed:matrix-scale)
  
  (: matrix-sum ((Listof (Matrix Number)) -> (Matrix Number)))
  (define matrix-sum typed:matrix-sum)
  
  )  ; module untyped-arithmetic-defs

(module arithmetic-defs racket/base
  (require typed/untyped-utils
           (prefix-in typed: "typed-matrix-arithmetic.rkt")
           (prefix-in untyped: (submod ".." untyped-arithmetic-defs))
           (rename-in "untyped-matrix-arithmetic.rkt"
                      [matrix-map  untyped:matrix-map]))
  
  (provide (all-defined-out))
  
  (define-typed/untyped-identifier matrix-map typed:matrix-map untyped:matrix-map)
  (define-typed/untyped-identifier matrix* typed:matrix* untyped:matrix*)
  (define-typed/untyped-identifier matrix+ typed:matrix+ untyped:matrix+)
  (define-typed/untyped-identifier matrix- typed:matrix- untyped:matrix-)
  (define-typed/untyped-identifier matrix-scale typed:matrix-scale untyped:matrix-scale)
  (define-typed/untyped-identifier matrix-sum typed:matrix-sum untyped:matrix-sum)
  
  )  ; module arithmetic-defs

(require (for-syntax racket/base)
         typed/untyped-utils
         (prefix-in typed: "typed-matrix-arithmetic.rkt")
         (prefix-in fun: (submod "." arithmetic-defs))
         (except-in "untyped-matrix-arithmetic.rkt" matrix-map)
         )

(define-syntax (define/inline-macro stx)
  (syntax-case stx ()
    [(_ name pat inline-fun typed:fun)
     (syntax/loc stx
       (define-syntax (name inner-stx)
         (syntax-case inner-stx ()
           [(_ . pat)  (syntax/loc inner-stx (inline-fun . pat))]
           [(_ . es)   (syntax/loc inner-stx (typed:fun . es))]
           [_          (syntax/loc inner-stx typed:fun)])))]))

(define/inline-macro matrix-map (f a as ...) inline-matrix-map fun:matrix-map)
(define/inline-macro matrix* (a as ...) inline-matrix* fun:matrix*)
(define/inline-macro matrix+ (a as ...) inline-matrix+ fun:matrix+)
(define/inline-macro matrix- (a as ...) inline-matrix- fun:matrix-)
(define/inline-macro matrix-scale (a x) inline-matrix-scale fun:matrix-scale)

(provide
 (rename-out [typed:matrix=  matrix=]
             [fun:matrix-sum  matrix-sum])
 matrix-map
 matrix*
 matrix+
 matrix-
 matrix-scale)
