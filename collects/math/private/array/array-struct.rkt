#lang typed/racket/base

(require racket/performance-hint
         (for-syntax racket/base racket/syntax)
         "../unsafe.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide Array (rename-out [-array?        array?]
                           [-lazy-array    lazy-array]
                           [-strict-array  strict-array])
         lazy-array? strict-array?
         ;; accessors and constructors
         (rename-out [safe-array-shape array-shape])
         array-dims
         array-size
         make-lazy-array
         make-strict-array
         unsafe-array-shape
         unsafe-lazy-array
         unsafe-strict-array
         unsafe-array-proc
         unsafe-array-data
         ;; conversion
         array-strict
         array-lazy
         ;; printing
         default-print-array
         array-custom-printer)

;; ===================================================================================================
;; Equality and hashing

(: lazy-array-equal? (All (A) ((lazy-array A) (lazy-array A) (Vectorof Index) (Any Any -> Boolean)
                                              -> Boolean)))
(define (lazy-array-equal? arr brr ds recur-equal?)
  (let/ec: return : Boolean
    (define f (unsafe-array-proc arr))
    (define g (unsafe-array-proc brr))
    (for-each-array-index ds (λ (js) (unless (recur-equal? (f js) (g js))
                                       (return #f))))
    #t))

(: mixed-array-equal? (All (A) ((lazy-array A) (strict-array A) (Vectorof Index) (Any Any -> Boolean)
                                               -> Boolean)))
(define (mixed-array-equal? arr brr ds recur-equal?)
  (let/ec: return : Boolean
    (define f (unsafe-array-proc arr))
    (define vs (unsafe-array-data brr))
    (for-each-array+data-index ds (λ (js j) (unless (recur-equal? (f js) (unsafe-vector-ref vs j))
                                              (return #f))))
    #t))

(: array-equal? (All (A) ((Array A) (Array A) (Any Any -> Boolean) -> Boolean)))
(define (array-equal? arr brr recur-equal?)
  (define ds (unsafe-array-shape arr))
  (and (equal? ds (unsafe-array-shape brr))
       (cond [(lazy-array? arr)
              (cond [(lazy-array? brr)  (lazy-array-equal? arr brr ds recur-equal?)]
                    [else  (mixed-array-equal? arr brr ds recur-equal?)])]
             [else
              (cond [(lazy-array? brr)  (mixed-array-equal? brr arr ds recur-equal?)]
                    [else  (recur-equal? (unsafe-array-data arr)
                                         (unsafe-array-data brr))])])))

(: array-hash-code (All (A) ((Array A) (Any -> Integer) -> Integer)))
(define (array-hash-code arr recur-hash-code)
  (cond [(lazy-array? arr)
         (define ds (unsafe-array-shape arr))
         (define f (unsafe-array-proc arr))
         (define h 0)
         (for-each-array-index ds (λ (js) (set! h (bitwise-xor h (recur-hash-code (f js))))))
         (bitwise-xor h (recur-hash-code ds))]
        [else
         (bitwise-xor (recur-hash-code (unsafe-array-shape arr))
                      (recur-hash-code (unsafe-array-data arr)))]))

;; ===================================================================================================
;; Parent array data type

(: array-guard ((Vectorof Index) Symbol -> (Vectorof Index)))
(define (array-guard ds name)
  (define size (unsafe-array-shape-size ds))
  (cond [(index? size)  ds]
        [else  (error name "array size ~e (shape ~e) is not an Index" size ds)]))

(struct: (A) array ([shape : (Vectorof Index)])
  #:property prop:equal+hash (list array-equal? array-hash-code array-hash-code)
  #:guard array-guard)

(: internal-array-size-error (All (A) ((Array A) Integer -> Nothing)))
(define (internal-array-size-error arr n)
  (error 'array-size "internal error: size of ~e should be an Index, but is ~e" arr n))

(begin-encourage-inline
  
  (: unsafe-array-shape (All (A) ((Array A) -> (Vectorof Index))))
  (define (unsafe-array-shape arr) (array-shape arr))
  
  (: array-dims (All (A) ((Array A) -> Index)))
  (define (array-dims arr) (vector-length (unsafe-array-shape arr)))
  
  (: array-size (All (A) ((Array A) -> Index)))
  (define (array-size arr)
    (define n (unsafe-array-shape-size (unsafe-array-shape arr)))
    (cond [(index? n)  n]
          [else  (internal-array-size-error arr n)]))
  
  (: safe-array-shape (All (A) ((Array A) -> (Listof Index))))
  (define (safe-array-shape arr) (array-shape-unsafe->safe (unsafe-array-shape arr)))
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; Lazy array data type

(struct: (A) lazy-array array ([proc : ((Vectorof Index) -> A)])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (arr port mode)
    ((array-custom-printer) arr port mode)))

(define unsafe-array-proc lazy-array-proc)

(: make-lazy-array (All (A) ((Listof Integer) ((Listof Index) -> A) -> (lazy-array A))))
(define (make-lazy-array ds proc)
  (let ([ds  (array-shape-safe->unsafe
              ds (λ () (raise-type-error 'lazy-array "(Listof Index)" 0 ds proc)))])
    (lazy-array ds (λ: ([js : (Vectorof Index)]) (proc (vector->list js))))))

;; A version of lazy-array that isn't also a type and a match expander
(: unsafe-lazy-array (All (A) ((Vectorof Index) ((Vectorof Index) -> A) -> (lazy-array A))))
(define (unsafe-lazy-array ds proc)
  (lazy-array ds proc))

;; ===================================================================================================
;; Strict array data type

(struct: (A) strict-array array ([data : (Vectorof A)])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write
  (λ (arr port mode)
    ((array-custom-printer) arr port mode)))

(: make-strict-array (All (A) ((Listof Integer) (Vectorof A) -> (strict-array A))))
(define (make-strict-array ds vs)
  (let ([ds  (array-shape-safe->unsafe
              ds (λ () (raise-type-error 'strict-array "(Listof Index)" 0 ds vs)))])
    (define size (unsafe-array-shape-size ds))
    (unless (= size (vector-length vs))
      (raise-type-error 'strict-array (format "Vector of length ~e" size)
                        1 (array-shape-unsafe->safe ds) vs))
    (strict-array ds vs)))

;; A version of strict-array that isn't also a type and a match expander
(: unsafe-strict-array (All (A) ((Vectorof Index) (Vectorof A) -> (strict-array A))))
(define (unsafe-strict-array ds vs)
  (strict-array ds vs))

(define unsafe-array-data strict-array-data)

;; ===================================================================================================
;; More types

(define-type (Array A) (U (lazy-array A) (strict-array A)))

;; Versions of lazy-array and strict-array that aren't also constructors and match expanders
(define-type (-lazy-array A) (lazy-array A))
(define-type (-strict-array A) (strict-array A))

;; Predicate for the union type (`array?' is different: identifies instances of array's descendants)
(begin-encourage-inline
  (define (-array? v) (or (lazy-array? v) (strict-array? v))))

;; ===================================================================================================
;; Lazy/strict conversion

(: array-lazy (All (A) ((Array A) -> (lazy-array A))))
(begin-encourage-inline
  (define (array-lazy arr)
    (cond [(lazy-array? arr)  arr]
          [else  (define ds (unsafe-array-shape arr))
                 (define vs (unsafe-array-data arr))
                 (unsafe-lazy-array
                  ds (λ: ([js : (Vectorof Index)])
                       (unsafe-vector-ref vs (unsafe-array-index->value-index ds js))))])))

(: array-strict (All (A) ((Array A) -> (strict-array A))))
(define (array-strict arr)
  (cond [(lazy-array? arr)
         (define ds (unsafe-array-shape arr))
         (define g (unsafe-array-proc arr))
         (define size (array-size arr))
         (unsafe-strict-array ds (inline-build-array-data ds (λ (js j) (g js))))]
        [else  arr]))

;; ===================================================================================================
;; Printing

(require racket/pretty)

(: default-print-array (All (A) ((Array A) Output-Port (U #t #f 0 1) -> Any)))
;; Mimicks the default custom printer for transparent struct values
(define (default-print-array arr port mode)
  (define col (port-next-column port))
  (define cols (pretty-print-columns))
  (define pp? (pretty-printing))
  
  (define recur-print
    (cond [(not mode) display]
          [(integer? mode) (λ: ([p : Any] [port : Output-Port])
                             (print p port mode))]
          [else write]))
  
  (define: (maybe-print-newline [indent : Integer]) : Any
    (cond [(and pp? (integer? cols))
           (pretty-print-newline port cols)
           (write-string (make-string (+ col indent) #\space) port)]
          [else
           (write-string " " port)]))
  
  (cond [(lazy-array? arr)
         (write-string "(lazy-array" port)
         (maybe-print-newline 1)
         (recur-print (unsafe-array-shape arr) port)
         (maybe-print-newline 1)
         (recur-print (unsafe-array-proc arr) port)
         (write-string ")" port)]
        [else
         (write-string "(strict-array" port)
         (maybe-print-newline 1)
         (recur-print (unsafe-array-shape arr) port)
         (maybe-print-newline 1)
         (recur-print (unsafe-array-data arr) port)
         (write-string ")" port)]))

;; In "array.rkt", this is set to `print-array' from from "array-print.rkt"
(: array-custom-printer (Parameterof (All (A) ((Array A) Output-Port (U #t #f 0 1) -> Any))))
(define array-custom-printer (make-parameter default-print-array))
