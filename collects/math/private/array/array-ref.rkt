#lang typed/racket/base

(require racket/unsafe/ops
         (for-syntax racket/base racket/syntax)
         "array-struct.rkt"
         "utils.rkt")

(provide unsafe-array-ref* unsafe-array-ref
         array-ref* array-ref)

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
