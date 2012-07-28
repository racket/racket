#lang typed/racket/base

(require (for-syntax racket/base racket/syntax)
         "../unsafe.rkt"
         "array-struct.rkt"
         "utils.rkt")

(provide unsafe-array-ref* unsafe-array-ref
         array-ref* array-ref
         unsafe-array-set!* unsafe-array-set!
         array-set!* array-set!)

;; ===================================================================================================
;; Unsafe array ref

(define-syntax (unsafe-indexes->index stx)
  (syntax-case stx ()
    [(_ ds i () j)  (syntax/loc stx j)]
    [(_ ds i (ji js ...) j)
     (with-syntax ([i+1  (+ (syntax->datum #'i) 1)])
       (syntax/loc stx
         (unsafe-indexes->index ds i+1 (js ...)
                                (unsafe-fx+ ji (unsafe-fx* (unsafe-vector-ref ds i) j)))))]))

(define-syntax (unsafe-array-ref* stx)
  (syntax-case stx ()
    [(_ arr-expr)
     (syntax/loc stx
       ((plambda: (A) ([arr : (Array A)])
          (if (lazy-array? arr)
              ((unsafe-array-proc arr) #())
              (unsafe-vector-ref (unsafe-array-data arr) 0)))
        arr-expr))]
    [(_ arr-expr j0 js ...)
     (with-syntax ([(new-j0 new-js ...)  (generate-temporaries #'(j0 js ...))])
       (syntax/loc stx
         ((plambda: (A) ([arr : (Array A)] [new-j0 : Index] [new-js : Index] ...)
            (let ([ds  (unsafe-array-shape arr)])
              (if (lazy-array? arr)
                  ((unsafe-array-proc arr) (vector new-j0 new-js ...))
                  (unsafe-vector-ref (unsafe-array-data arr)
                                     (unsafe-indexes->index ds 1 (new-js ...) new-j0)))))
           arr-expr j0 js ...)))]
    [_  (syntax/loc stx unsafe-array-ref*-proc)]))

(: unsafe-array-ref*-proc
   (All (A) (case-> ((Array A) -> A)
                    ((Array A) Index -> A)
                    ((Array A) Index Index -> A)
                    ((Array A) Index Index Index Index * -> A))))
(define unsafe-array-ref*-proc
  (case-lambda:
    [([arr : (Array A)])
     (unsafe-array-ref* arr)]
    [([arr : (Array A)] [j0 : Index])
     (unsafe-array-ref* arr j0)]
    [([arr : (Array A)] [j0 : Index] [j1 : Index])
     (unsafe-array-ref* arr j0 j1)]
    [([arr : (Array A)] [j0 : Index] [j1 : Index] [j2 : Index])
     (unsafe-array-ref* arr j0 j1 j2)]
    [([arr : (Array A)] [j0 : Index] [j1 : Index] [j2 : Index] . [js : Index *])
     (unsafe-array-ref arr ((inst list->vector Index) (list* j0 j1 j2 js)))]))

(: unsafe-array-ref (All (A) ((Array A) (Vectorof Index) -> A)))
(define (unsafe-array-ref arr js)
  (if (lazy-array? arr)
      ((unsafe-array-proc arr) js)
      (unsafe-vector-ref (unsafe-array-data arr)
                         (unsafe-array-index->value-index (unsafe-array-shape arr) js))))

;; ===================================================================================================
;; Safe array ref

(define-syntax (indexes->index stx)
  (syntax-case stx ()
    [(_ () () j)  (syntax/loc stx j)]
    [(_ (di ds ...) (ji js ...) j)
     (syntax/loc stx
       (indexes->index (ds ...) (js ...)
                       (unsafe-fx+ ji (unsafe-fx* di j))))]))

(define-syntax (array-ref* stx)
  (syntax-case stx ()
    [(_ arr-expr)
     (syntax/loc stx
       ((plambda: (A) ([arr : (Array A)])
          (if (= 0 (array-dims arr))
              (if (lazy-array? arr)
                  ((unsafe-array-proc arr) #())
                  (unsafe-vector-ref (unsafe-array-data arr) 0))
              (raise-array-index-error 'array-ref* (unsafe-array-shape arr) null)))
        arr-expr))]
    [(_ arr-expr j0 js ...)
     (with-syntax* ([len  (length (syntax->list #'(j0 js ...)))]
                    [(new-j0 new-js ...)  (generate-temporaries #'(j0 js ...))]
                    [(new-ds ...)  (generate-temporaries #'(js ...))]
                    [(is ...)  (build-list (- (syntax->datum #'len) 1) add1)])
       (syntax/loc stx
         ((plambda: (A) ([arr : (Array A)] [new-j0 : Integer] [new-js : Integer] ...)
            (let* ([ds  (unsafe-array-shape arr)]
                   [index-error  (λ () (raise-array-index-error
                                        'array-ref* ds (list new-j0 new-js ...)))])
              (if (= (vector-length ds) len)
                  (let-values ([(new-ds ...)  (values (unsafe-vector-ref ds is) ...)])
                    (if (and (and (0 . <= . new-j0) (new-j0 . < . (unsafe-vector-ref ds 0)))
                             (and (0 . <= . new-js) (new-js . < . new-ds)) ...)
                        (if (lazy-array? arr)
                            ((unsafe-array-proc arr) (vector new-j0 new-js ...))
                            (let ([j  (indexes->index (new-ds ...) (new-js ...) new-j0)])
                              (unsafe-vector-ref (unsafe-array-data arr) j)))
                        (index-error)))
                  (index-error))))
          arr-expr j0 js ...)))]
    [_  (syntax/loc stx array-ref*-proc)]))

(: array-ref*-proc
   (All (A) (case-> ((Array A) -> A)
                    ((Array A) Integer -> A)
                    ((Array A) Integer Integer -> A)
                    ((Array A) Integer Integer Integer Integer * -> A))))
(define array-ref*-proc
  (case-lambda:
    [([arr : (Array A)])
     (array-ref* arr)]
    [([arr : (Array A)] [j0 : Integer])
     (array-ref* arr j0)]
    [([arr : (Array A)] [j0 : Integer] [j1 : Integer])
     (array-ref* arr j0 j1)]
    [([arr : (Array A)] [j0 : Integer] [j1 : Integer] [j2 : Integer])
     (array-ref* arr j0 j1 j2)]
    [([arr : (Array A)] [j0 : Integer] [j1 : Integer] [j2 : Integer] . [js : Integer *])
     (array-ref arr (list* j0 j1 j2 js))]))

(: array-ref (All (A) ((Array A) (Listof Integer) -> A)))
(define (array-ref arr js)
  (define ds (unsafe-array-shape arr))
  (if (lazy-array? arr)
      ((unsafe-array-proc arr) (check-array-indexes 'array-ref ds js))
      (unsafe-vector-ref (unsafe-array-data arr)
                         (array-index->value-index 'array-ref ds js))))

;; ===================================================================================================
;; Unsafe array set

(define-syntax (unsafe-array-set!* stx)
  (syntax-case stx ()
    [(_ arr-expr v-expr)
     (syntax/loc stx
       ((plambda: (A) ([arr : (strict-array A)] [v : A])
          (unsafe-vector-set! (unsafe-array-data arr) 0 v))
        arr-expr v-expr))]
    [(_ arr-expr v-expr j0 js ...)
     (with-syntax ([(new-j0 new-js ...)  (generate-temporaries #'(j0 js ...))])
       (syntax/loc stx
         ((plambda: (A) ([arr : (strict-array A)] [v : A] [new-j0 : Index] [new-js : Index] ...)
            (let ([ds  (unsafe-array-shape arr)])
              (unsafe-vector-set! (unsafe-array-data arr)
                                  (unsafe-indexes->index ds 1 (new-js ...) new-j0)
                                  v)))
           arr-expr v-expr j0 js ...)))]
    [_  (syntax/loc stx unsafe-array-set!*-proc)]))

(: unsafe-array-set!*-proc
   (All (A) (case-> ((strict-array A) A -> Void)
                    ((strict-array A) A Index -> Void)
                    ((strict-array A) A Index Index -> Void)
                    ((strict-array A) A Index Index Index Index * -> Void))))
(define unsafe-array-set!*-proc
  (case-lambda:
    [([arr : (strict-array A)] [v : A])
     (unsafe-array-set!* arr v)]
    [([arr : (strict-array A)] [v : A] [j0 : Index])
     (unsafe-array-set!* arr v j0)]
    [([arr : (strict-array A)] [v : A] [j0 : Index] [j1 : Index])
     (unsafe-array-set!* arr v j0 j1)]
    [([arr : (strict-array A)] [v : A] [j0 : Index] [j1 : Index] [j2 : Index])
     (unsafe-array-set!* arr v j0 j1 j2)]
    [([arr : (strict-array A)] [v : A] [j0 : Index] [j1 : Index] [j2 : Index] . [js : Index *])
     (unsafe-array-set! arr ((inst list->vector Index) (list* j0 j1 j2 js)) v)]))

(: unsafe-array-set! (All (A) ((strict-array A) (Vectorof Index) A -> Void)))
(define (unsafe-array-set! arr js v)
  (unsafe-vector-set! (unsafe-array-data arr)
                      (unsafe-array-index->value-index (unsafe-array-shape arr) js)
                      v))

;; ===================================================================================================
;; Safe array set

(define-syntax (array-set!* stx)
  (syntax-case stx ()
    [(_ arr-expr v-expr)
     (syntax/loc stx
       ((plambda: (A) ([arr : (strict-array A)] [v : A])
          (if (= 0 (array-dims arr))
              (unsafe-vector-set! (unsafe-array-data arr) 0 v)
              (raise-array-index-error 'array-set!* (unsafe-array-shape arr) null)))
        arr-expr v-expr))]
    [(_ arr-expr v-expr j0 js ...)
     (with-syntax* ([len  (length (syntax->list #'(j0 js ...)))]
                    [(new-j0 new-js ...)  (generate-temporaries #'(j0 js ...))]
                    [(new-ds ...)  (generate-temporaries #'(js ...))]
                    [(is ...)  (build-list (- (syntax->datum #'len) 1) add1)])
       (syntax/loc stx
         ((plambda: (A) ([arr : (strict-array A)] [v : A] [new-j0 : Integer] [new-js : Integer] ...)
            (let* ([ds  (unsafe-array-shape arr)]
                   [index-error  (λ () (raise-array-index-error
                                        'array-set!* ds (list new-j0 new-js ...)))])
              (if (= (vector-length ds) len)
                  (let-values ([(new-ds ...)  (values (unsafe-vector-ref ds is) ...)])
                    (if (and (and (0 . <= . new-j0) (new-j0 . < . (unsafe-vector-ref ds 0)))
                             (and (0 . <= . new-js) (new-js . < . new-ds)) ...)
                        (let ([j  (indexes->index (new-ds ...) (new-js ...) new-j0)])
                          (unsafe-vector-set! (unsafe-array-data arr) j v))
                        (index-error)))
                  (index-error))))
          arr-expr v-expr j0 js ...)))]
    [_  (syntax/loc stx array-set!*-proc)]))

(: array-set!*-proc
   (All (A) (case-> ((strict-array A) A -> Void)
                    ((strict-array A) A Integer -> Void)
                    ((strict-array A) A Integer Integer -> Void)
                    ((strict-array A) A Integer Integer Integer Integer * -> Void))))
(define array-set!*-proc
  (case-lambda:
    [([arr : (strict-array A)] [v : A])
     (array-set!* arr v)]
    [([arr : (strict-array A)] [v : A] [j0 : Integer])
     (array-set!* arr v j0)]
    [([arr : (strict-array A)] [v : A] [j0 : Integer] [j1 : Integer])
     (array-set!* arr v j0 j1)]
    [([arr : (strict-array A)] [v : A] [j0 : Integer] [j1 : Integer] [j2 : Integer])
     (array-set!* arr v j0 j1 j2)]
    [([arr : (strict-array A)] [v : A] [j0 : Integer] [j1 : Integer] [j2 : Integer] . [js : Integer *])
     (array-set! arr (list* j0 j1 j2 js) v)]))

(: array-set! (All (A) ((strict-array A) (Listof Integer) A -> Void)))
(define (array-set! arr js v)
  (define ds (unsafe-array-shape arr))
  (unsafe-vector-set! (unsafe-array-data arr)
                      (array-index->value-index 'array-set! ds js)
                      v))
