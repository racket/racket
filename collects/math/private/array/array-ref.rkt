#lang typed/racket/base

(require (for-syntax racket/base racket/syntax)
         racket/performance-hint
         "../unsafe.rkt"
         "array-struct.rkt"
         "utils.rkt")

(provide (rename-out [inline-unsafe-array-ref   unsafe-array-ref]
                     [inline-unsafe-array-set!  unsafe-array-set!]
                     [inline-array-ref   array-ref]
                     [inline-array-set!  array-set!]))

;; ===================================================================================================
;; Unsafe array ref/set!

(define-syntax (unsafe-indexes->index stx)
  (syntax-case stx ()
    [(_ ds i () j)  (syntax/loc stx j)]
    [(_ ds i (ji js ...) j)
     (with-syntax ([i+1  (+ (syntax->datum #'i) 1)])
       (syntax/loc stx
         (unsafe-indexes->index ds i+1 (js ...)
                                (unsafe-fx+ ji (unsafe-fx* (unsafe-vector-ref ds i) j)))))]))

(define-syntax (inline-unsafe-array-ref* stx)
  (syntax-case stx ()
    [(_ arr-expr)
     (syntax/loc stx
       (plet: (A) ([arr : (Array A)  arr-expr])
         (if (view-array? arr)
             ((unsafe-array-proc arr) empty-vectorof-index)
             (unsafe-vector-ref (strict-array-data arr) 0))))]
    [(_ arr-expr j0-expr js-expr ...)
     (with-syntax ([(j0 js ...)  (generate-temporaries #'(j0-expr js-expr ...))])
       (syntax/loc stx
         (plet: (A) ([arr : (Array A)  arr-expr] [j0 : Index  j0-expr] [js : Index  js-expr] ...)
           (let ([ds  (unsafe-array-shape arr)])
             (if (view-array? arr)
                 ((unsafe-array-proc arr) (vector j0 js ...))
                 (unsafe-vector-ref (strict-array-data arr)
                                    (unsafe-indexes->index ds 1 (js ...) j0)))))))]))

(define-syntax (inline-unsafe-array-set!* stx)
  (syntax-case stx ()
    [(_ arr-expr v-expr)
     (syntax/loc stx
       (plet: (A) ([arr : (strict-array A)  arr-expr] [v : A  v-expr])
         (unsafe-vector-set! (strict-array-data arr) 0 v)))]
    [(_ arr-expr v-expr j0-expr js-expr ...)
     (with-syntax ([(j0 js ...)  (generate-temporaries #'(j0-expr js-expr ...))])
       (syntax/loc stx
         (plet: (A) ([arr : (strict-array A)  arr-expr]
                     [v : A  v-expr]
                     [j0 : Index  j0-expr] [js : Index  js-expr] ...)
           (let ([ds  (unsafe-array-shape arr)])
             (unsafe-vector-set! (strict-array-data arr)
                                 (unsafe-indexes->index ds 1 (js ...) j0)
                                 v)))))]))

(define-syntax (inline-unsafe-array-ref stx)
  (syntax-case stx ()
    [(_ arr js)
     (let ([js-stx  (local-expand #'js (syntax-local-context) null)])
       (syntax-case js-stx (#%plain-app vector quote)
         [(#%plain-app vector j ...)
          (syntax/loc stx (begin (ann js (Vectorof Index))
                                 (inline-unsafe-array-ref* arr j ...)))]
         [(quote #(j ...))
          (andmap integer? (syntax->datum #'(j ...)))
          (syntax/loc stx (begin (ann js (Vectorof Index))
                                 (inline-unsafe-array-ref* arr j ...)))]
         [_
          (syntax/loc stx (unsafe-array-ref arr js))]))]
    [(_ e ...)
     (syntax/loc stx (unsafe-array-ref e ...))]
    [_
     (syntax/loc stx unsafe-array-ref)]))

(define-syntax (inline-unsafe-array-set! stx)
  (syntax-case stx ()
    [(_ arr js v)
     (let ([js-stx  (local-expand #'js (syntax-local-context) null)])
       (syntax-case js-stx (#%plain-app vector quote)
         [(#%plain-app vector j ...)
          (syntax/loc stx (begin (ann js (Vectorof Index))
                                 (inline-unsafe-array-set!* arr v j ...)))]
         [(quote #(j ...))
          (andmap integer? (syntax->datum #'(j ...)))
          (syntax/loc stx (begin (ann js (Vectorof Index))
                                 (inline-unsafe-array-set!* arr v j ...)))]
         [_
          (syntax/loc stx (unsafe-array-set! arr js v))]))]
    [(_ e ...)
     (syntax/loc stx (unsafe-array-set! e ...))]
    [_
     (syntax/loc stx unsafe-array-set!)]))

(: unsafe-array-ref (All (A) ((Array A) (Vectorof Index) -> A)))
(begin-encourage-inline
  (define (unsafe-array-ref arr js)
    (if (view-array? arr)
        ((unsafe-array-proc arr) js)
        (unsafe-vector-ref (strict-array-data arr)
                           (unsafe-array-index->value-index (unsafe-array-shape arr) js)))))

(: unsafe-array-set! (All (A) ((strict-array A) (Vectorof Index) A -> Void)))
(begin-encourage-inline
  (define (unsafe-array-set! arr js v)
    (unsafe-vector-set! (strict-array-data arr)
                        (unsafe-array-index->value-index (unsafe-array-shape arr) js)
                        v)))

;; ===================================================================================================
;; Safe array ref/set!

(define-syntax (indexes->index stx)
  (syntax-case stx ()
    [(_ () () j)  (syntax/loc stx j)]
    [(_ (di ds ...) (ji js ...) j)
     (syntax/loc stx
       (indexes->index (ds ...) (js ...)
                       (unsafe-fx+ ji (unsafe-fx* di j))))]))

(define-syntax (inline-array-ref* stx)
  (syntax-case stx ()
    [(_ arr-expr)
     (syntax/loc stx
       (plet: (A) ([arr : (Array A)  arr-expr])
         (if (= 0 (array-dims arr))
             (if (view-array? arr)
                 ((unsafe-array-proc arr) empty-vectorof-index)
                 (unsafe-vector-ref (strict-array-data arr) 0))
             (raise-array-index-error 'array-ref* (unsafe-array-shape arr) null))))]
    [(_ arr-expr j0-expr js-expr ...)
     (with-syntax* ([dims  (length (syntax->list #'(j0-expr js-expr ...)))]
                    [(j0 js ...)  (generate-temporaries #'(j0-expr js-expr ...))]
                    [(ds ...)  (generate-temporaries #'(js-expr ...))]
                    [(is ...)  (build-list (- (syntax->datum #'dims) 1) add1)])
       (syntax/loc stx
         (plet: (A) ([arr : (Array A)  arr-expr] [j0 : Integer  j0-expr] [js : Integer  js-expr] ...)
           (let* ([ds-vec  (unsafe-array-shape arr)]
                  [index-error  (λ () (raise-array-index-error
                                       'array-ref* ds-vec (list j0 js ...)))])
             (if (= (vector-length ds-vec) dims)
                 (let-values ([(ds ...)  (values (unsafe-vector-ref ds-vec is) ...)])
                   (if (and (and (0 . <= . j0) (j0 . < . (unsafe-vector-ref ds-vec 0)))
                            (and (0 . <= . js) (js . < . ds)) ...)
                       (if (view-array? arr)
                           ((unsafe-array-proc arr) (vector j0 js ...))
                           (let ([j  (indexes->index (ds ...) (js ...) j0)])
                             (unsafe-vector-ref (strict-array-data arr) j)))
                       (index-error)))
                 (index-error))))))]))

(define-syntax (inline-array-set!* stx)
  (syntax-case stx ()
    [(_ arr-expr v-expr)
     (syntax/loc stx
       (plet: (A) ([arr : (strict-array A)  arr-expr] [v : A  v-expr])
         (if (= 0 (array-dims arr))
             (unsafe-vector-set! (strict-array-data arr) 0 v)
             (raise-array-index-error 'array-set!* (unsafe-array-shape arr) null))))]
    [(_ arr-expr v-expr j0-expr js-expr ...)
     (with-syntax* ([dims  (length (syntax->list #'(j0-expr js-expr ...)))]
                    [(j0 js ...)  (generate-temporaries #'(j0-expr js-expr ...))]
                    [(ds ...)  (generate-temporaries #'(js-expr ...))]
                    [(is ...)  (build-list (- (syntax->datum #'dims) 1) add1)])
       (syntax/loc stx
         (plet: (A) ([arr : (strict-array A)  arr-expr]
                     [v : A  v-expr]
                     [j0 : Integer  j0-expr] [js : Integer  js-expr] ...)
           (let* ([ds-vec  (unsafe-array-shape arr)]
                  [index-error  (λ () (raise-array-index-error
                                       'array-set!* ds-vec (list j0 js ...)))])
             (if (= (vector-length ds-vec) dims)
                 (let-values ([(ds ...)  (values (unsafe-vector-ref ds-vec is) ...)])
                   (if (and (and (0 . <= . j0) (j0 . < . (unsafe-vector-ref ds-vec 0)))
                            (and (0 . <= . js) (js . < . ds)) ...)
                       (let ([j  (indexes->index (ds ...) (js ...) j0)])
                         (unsafe-vector-set! (strict-array-data arr) j v))
                       (index-error)))
                 (index-error))))))]))

(define-syntax (inline-array-ref stx)
  (syntax-case stx ()
    [(_ arr js)
     (let ([js-stx  (local-expand #'js (syntax-local-context) null)])
       (syntax-case js-stx (#%plain-app list quote)
         [(#%plain-app list j ...)
          (syntax/loc stx (begin (ann js (Listof Integer))
                                 (inline-array-ref* arr j ...)))]
         [(quote (j ...))
          (andmap integer? (syntax->datum #'(j ...)))
          (syntax/loc stx (begin (ann js (Listof Integer))
                                 (inline-array-ref* arr j ...)))]
         [_
          (syntax/loc stx (array-ref arr js))]))]
    [(_ e ...)
     (syntax/loc stx (array-ref e ...))]
    [_
     (syntax/loc stx array-ref)]))

(define-syntax (inline-array-set! stx)
  (syntax-case stx ()
    [(_ arr js v)
     (let ([js-stx  (local-expand #'js (syntax-local-context) null)])
       (syntax-case js-stx (#%plain-app list quote)
         [(#%plain-app list j ...)
          (syntax/loc stx (begin (ann js (Listof Integer))
                                 (inline-array-set!* arr v j ...)))]
         [(quote (j ...))
          (andmap integer? (syntax->datum #'(j ...)))
          (syntax/loc stx (begin (ann js (Listof Integer))
                                 (inline-array-set!* arr v j ...)))]
         [_
          (syntax/loc stx (array-set! arr js v))]))]
    [(_ e ...)
     (syntax/loc stx (array-set! e ...))]
    [_
     (syntax/loc stx array-set!)]))

(: array-ref (All (A) ((Array A) (Listof Integer) -> A)))
(begin-encourage-inline
  (define (array-ref arr js)
    (define ds (unsafe-array-shape arr))
    (if (view-array? arr)
        ((unsafe-array-proc arr) (check-array-indexes 'array-ref ds js))
        (unsafe-vector-ref (strict-array-data arr)
                           (array-index->value-index 'array-ref ds js)))))

(: array-set! (All (A) ((strict-array A) (Listof Integer) A -> Void)))
(begin-encourage-inline
  (define (array-set! arr js v)
    (define ds (unsafe-array-shape arr))
    (unsafe-vector-set! (strict-array-data arr)
                        (array-index->value-index 'array-set! ds js)
                        v)))
