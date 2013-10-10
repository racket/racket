#lang racket/base

(provide inline-matrix-multiply
         inline-matrix*
         inline-matrix+
         inline-matrix-
         inline-matrix-scale
         inline-matrix-map
         matrix-map)

(module typed-multiply-defs typed/racket/base
  (require racket/fixnum
           "matrix-types.rkt"
           "utils.rkt"
           "../unsafe.rkt"
           "../array/array-struct.rkt"
           "../array/array-transform.rkt"
           "../array/mutable-array.rkt"
           "../array/utils.rkt")
  
  (provide (all-defined-out))
  
  (: matrix-multiply-data (All (A) ((Matrix A) (Matrix A) -> (Values Index Index Index
                                                                     (Vectorof A) (Vectorof A)
                                                                     (-> (Boxof A))))))
  (define (matrix-multiply-data arr brr)
    (let-values ([(m p n)  (matrix-multiply-shape arr brr)])
      (define arr-data (mutable-array-data (array->mutable-array arr)))
      (define brr-data (mutable-array-data (array->mutable-array (parameterize ([array-strictness #f])
                                                                   (array-axis-swap brr 0 1)))))
      (define bx (make-thread-local-box (unsafe-vector-ref arr-data 0)))
      (values m p n arr-data brr-data bx)))
  
  (: make-matrix-multiply (All (A) (Index Index Index (Index Index -> A) -> (Matrix A))))
  (define (make-matrix-multiply m p n sum-loop)
    (array-default-strict
     (unsafe-build-array
      ((inst vector Index) m n)
      (λ: ([ij : Indexes])
        (sum-loop (assert (fx* (unsafe-vector-ref ij 0) p) index?)
                  (assert (fx* (unsafe-vector-ref ij 1) p) index?))))))
  )  ; module

(module untyped-multiply-defs racket/base
  (require (for-syntax racket/base)
           racket/fixnum
           racket/unsafe/ops
           (only-in typed/racket/base λ: : Index let: Nonnegative-Fixnum)
           (submod ".." typed-multiply-defs)
           "matrix-types.rkt"
           "utils.rkt"
           "../array/array-struct.rkt")
  
  (provide (all-defined-out))
  
  ;; This is a macro so the result can have as precise a type as possible
  (define-syntax-rule (inline-matrix-multiply arr brr)
    (let-values ([(m p n arr-data brr-data bx)  (matrix-multiply-data arr brr)])
      (make-matrix-multiply
       m p n
       (λ: ([i : Index] [j : Index])
         (let ([bx  (bx)]
               [v  (* (unsafe-vector-ref arr-data i)
                      (unsafe-vector-ref brr-data j))])
           (let: loop ([k : Nonnegative-Fixnum  1] [v v])
             (cond [(k . fx< . p)
                    (loop (fx+ k 1)
                          (+ v (* (unsafe-vector-ref arr-data (fx+ i k))
                                  (unsafe-vector-ref brr-data (fx+ j k)))))]
                   [else  (set-box! bx v)]))
           (unbox bx))))))
  
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
  
  )  ; module

(module syntax-defs racket/base
  (require (for-syntax racket/base)
           (only-in typed/racket/base λ: : inst Index)
           (submod ".." typed-multiply-defs)
           "matrix-types.rkt"
           "utils.rkt"
           "../array/array-struct.rkt"
           "../array/utils.rkt")
  
  (provide (all-defined-out))
    
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

(require 'untyped-multiply-defs
         'syntax-defs
         'untyped-defs)
