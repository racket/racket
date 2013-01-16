#lang racket/base

(provide inline-matrix-multiply
         inline-matrix*
         inline-matrix+
         inline-matrix-
         inline-matrix-scale
         inline-matrix-map
         matrix-map)

(module syntax-defs racket/base
  (require (for-syntax racket/base)
           (only-in typed/racket/base λ: : inst Index)
           "matrix-types.rkt"
           "utils.rkt"
           "../array/array-struct.rkt"
           "../array/array-fold.rkt"
           "../array/array-transform.rkt"
           "../array/utils.rkt")
  
  (provide (all-defined-out))
  
  ;(: matrix-multiply ((Matrix Number) (Matrix Number) -> (Matrix Number)))
  ;; This is a macro so the result can have as precise a type as possible
  (define-syntax-rule (inline-matrix-multiply arr-expr brr-expr)
    (let ([arr  arr-expr]
          [brr  brr-expr])
      (let-values ([(m p n)  (matrix-multiply-shape arr brr)]
                   ;; Make arr strict because its elements are reffed multiple times
                   [(_)  (array-strict! arr)])
        (let (;; Extend arr in the center dimension
              [arr-proc  (unsafe-array-proc (array-axis-insert arr 1 n))]
              ;; Transpose brr and extend in the leftmost dimension
              [brr-proc  (unsafe-array-proc
                          (array-axis-insert (array-strict (array-axis-swap brr 0 1)) 0 m))])
          ;; The *transpose* of brr is traversed in row-major order when this result is traversed
          ;; in row-major order (which is why the transpose is strict, not brr)
          (array-axis-sum
           (unsafe-build-array
            ((inst vector Index) m n p)
            (λ: ([js : Indexes])
              (* (arr-proc js) (brr-proc js))))
           2)))))
  
  (define-syntax (do-inline-matrix* stx)
    (syntax-case stx ()
      [(_ arr)
       (syntax/loc stx arr)]
      [(_ arr brr crrs ...)
       (syntax/loc stx (do-inline-matrix* (inline-matrix-multiply arr brr) crrs ...))]))
  
  (define-syntax-rule (inline-matrix* arr brrs ...)
    (array-default-strict
     (parameterize ([array-strictness #f])
       (do-inline-matrix* arr brrs ...))))
  
  (define-syntax (inline-matrix-map stx)
    (syntax-case stx ()
      [(_ f arr-expr)
       (syntax/loc stx
         (let*-values ([(arr)  arr-expr]
                       [(m n)  (matrix-shape arr)]
                       [(proc)  (unsafe-array-proc arr)])
           (array-default-strict
            (unsafe-build-array ((inst vector Index) m n) (λ: ([js : Indexes]) (f (proc js)))))))]
      [(_ f arr-expr brr-exprs ...)
       (with-syntax ([(brrs ...)  (generate-temporaries #'(brr-exprs ...))]
                     [(procs ...)  (generate-temporaries #'(brr-exprs ...))])
         (syntax/loc stx
           (let ([arr arr-expr]
                 [brrs brr-exprs] ...)
             (let-values ([(m n)  (matrix-shapes 'matrix-map arr brrs ...)]
                          [(proc)  (unsafe-array-proc arr)]
                          [(procs)  (unsafe-array-proc brrs)] ...)
               (array-default-strict
                (unsafe-build-array
                 ((inst vector Index) m n)
                 (λ: ([js : Indexes])
                   (f (proc js) (procs js) ...))))))))]))
  
  (define-syntax-rule (inline-matrix+ arr0 arrs ...) (inline-matrix-map + arr0 arrs ...))
  (define-syntax-rule (inline-matrix- arr0 arrs ...) (inline-matrix-map - arr0 arrs ...))
  (define-syntax-rule (inline-matrix-scale arr x-expr)
    (let ([x x-expr])
      (inline-matrix-map (λ (y) (* x y)) arr)))
  
  )  ; module

(module untyped-defs typed/racket/base
  (require (submod ".." syntax-defs)
           "matrix-types.rkt"
           "utils.rkt"
           "../array/array-struct.rkt"
           "../array/utils.rkt")
  
  (provide matrix-map)
  
  (: matrix-map
     (All (R A) (case-> ((A -> R) (Matrix A) -> (Matrix R))
                        ((A A A * -> R) (Matrix A) (Matrix A) (Matrix A) * -> (Matrix R)))))
  (define matrix-map
    (case-lambda:
      [([f : (A -> R)] [arr : (Matrix A)])
       (inline-matrix-map f arr)]
      [([f : (A A -> R)] [arr0 : (Matrix A)] [arr1 : (Matrix A)])
       (inline-matrix-map f arr0 arr1)]
      [([f : (A A A * -> R)] [arr0 : (Matrix A)] [arr1 : (Matrix A)] . [arrs : (Matrix A) *])
       (define-values (m n) (apply matrix-shapes 'matrix-map arr0 arr1 arrs))
       (define g0 (unsafe-array-proc arr0))
       (define g1 (unsafe-array-proc arr1))
       (define gs (map (inst unsafe-array-proc A) arrs))
       (array-default-strict
        (unsafe-build-array
         ((inst vector Index) m n)
         (λ: ([js : Indexes]) (apply f (g0 js) (g1 js)
                                     (map (λ: ([g : (Indexes -> A)]) (g js)) gs)))))]))
  
  )  ; module

(require 'syntax-defs
         'untyped-defs)
