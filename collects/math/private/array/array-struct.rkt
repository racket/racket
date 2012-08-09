#lang typed/racket/base

(require racket/promise
         racket/performance-hint
         (for-syntax racket/base racket/syntax)
         "../unsafe.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide Array
         (rename-out [-array?        array?]
                     [-View-Array    View-Array]
                     [-Strict-Array  Strict-Array])
         view-array? strict-array?
         ;; accessors and constructors
         array-shape
         array-dims
         array-size
         make-view-array
         make-strict-array
         unsafe-view-array
         unsafe-strict-array
         unsafe-array-proc
         strict-array-data
         ;; equality testing
         array-lift-comparison
         ;; conversion
         array-view
         array-strict
         array-copy
         array-lazy
         ;; printing
         print-array-fields
         array-custom-printer
         ; matrix
         flat-vector->matrix)

;; ===================================================================================================
;; Equality and hashing

(: array-recur-equal? ((Array Any) (Array Any) (Any Any -> Boolean) -> Boolean))
(define (array-recur-equal? arr brr recur-equal?)
  ((array-lift-comparison recur-equal?) arr brr))

(: array-hash-code (All (A) ((Array A) (Any -> Integer) -> Integer)))
(define (array-hash-code arr recur-hash-code)
  (cond [(view-array? arr)
         (define ds (array-shape arr))
         (define f (unsafe-array-proc arr))
         (define h 0)
         (for-each-array-index ds (λ (js) (set! h (bitwise-xor h (recur-hash-code (f js))))))
         (bitwise-xor h (recur-hash-code ds))]
        [else
         (bitwise-xor (recur-hash-code (array-shape arr))
                      (recur-hash-code (strict-array-data arr)))]))

;; ===================================================================================================
;; Parent array data type

(: array-guard (Indexes Index Symbol -> (Values Indexes Index)))
(define (array-guard ds size name)
  (cond [(zero? size)
         (let ([size  (array-shape-size ds)])
           (cond [(index? size)  (values (vector->immutable-vector ds) size)]
                 [else  (error 'array "array size ~e (for shape ~e) is too large (is not an Index)"
                               size ds)]))]
        [else  (values (vector->immutable-vector ds) size)]))

(struct: (A) array ([shape : Indexes] [size : Index])
  #:property prop:equal+hash (list array-recur-equal? array-hash-code array-hash-code)
  #:guard array-guard)

(: array-dims (All (A) ((Array A) -> Index)))
(begin-encourage-inline
  (define (array-dims arr) (vector-length (array-shape arr))))

;; ===================================================================================================
;; View array data type

(struct: (A) View-Array array ([proc : (Indexes -> A)])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (λ (arr port mode) ((array-custom-printer) arr port mode)))

(define view-array? View-Array?)
(define unsafe-array-proc View-Array-proc)

(: make-view-array (All (A) (User-Indexes (Indexes -> A) -> (View-Array A))))
(define (make-view-array ds proc)
  (let ([ds  (check-array-shape
              ds (λ () (raise-type-error 'make-view-array "(Vectorof Index)" 0 ds proc)))])
    (View-Array ds 0 (λ: ([js : Indexes])
                       (proc (vector->immutable-vector js))))))

(: unsafe-view-array (All (A) (Indexes (Indexes -> A) -> (View-Array A))))
(define (unsafe-view-array ds proc)
  (View-Array ds 0 proc))

;; ===================================================================================================
;; Strict array data type

(struct: (A) Strict-Array array ([data : (Vectorof A)])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (λ (arr port mode) ((array-custom-printer) arr port mode)))

(define strict-array? Strict-Array?)
(define strict-array-data Strict-Array-data)

(: make-strict-array (All (A) (User-Indexes (Vectorof A) -> (Strict-Array A))))
(define (make-strict-array ds vs)
  (let* ([ds  (check-array-shape
               ds (λ () (raise-type-error 'strict-array "(Vectorof Index)" 0 ds vs)))]
         [size  (array-shape-size ds)]
         [n  (vector-length vs)])
    (cond [(= size n)  (Strict-Array ds n vs)]
          [else  (raise-type-error 'strict-array (format "Vector of length ~e" size) 1 ds vs)])))

(: unsafe-strict-array (All (A) (Indexes (Vectorof A) -> (Strict-Array A))))
(define (unsafe-strict-array ds vs)
  (Strict-Array ds (vector-length vs) vs))

(: flat-vector->matrix : (All (A) (Index Index (Vectorof A) -> (View-Array A))))
(define (flat-vector->matrix m n v)
  (array-view (make-strict-array (vector m n) v)))

;; ===================================================================================================
;; More types

(define-type (Array A) (U (View-Array A) (Strict-Array A)))

;; Versions of view-array and strict-array that aren't also constructors and match expanders
(define-type (-View-Array A) (View-Array A))
(define-type (-Strict-Array A) (Strict-Array A))

;; Predicate for the union type (`array?' is different: identifies instances of array's descendants)
(begin-encourage-inline
  (define (-array? v) (or (view-array? v) (strict-array? v))))

;; ===================================================================================================
;; View/strict conversion

(begin-encourage-inline

  (: array-view (All (A) ((Array A) -> (View-Array A))))
  (define (array-view arr)
    (cond [(view-array? arr)  arr]
          [else
           (define ds (array-shape arr))
           (define vs (strict-array-data arr))
           (unsafe-view-array
            ds (λ: ([js : Indexes])
                 (unsafe-vector-ref vs (unsafe-array-index->value-index ds js))))]))
  
  (: array-copy (All (A) ((Array A) -> (Strict-Array A))))
  (define (array-copy arr)
    (cond [(view-array? arr)  (array-strict arr)]
          [else  (unsafe-strict-array (array-shape arr)
                                      (vector-copy-all (strict-array-data arr)))]))
  
  )  ; begin-encourage-inline

(: array-strict (All (A) ((Array A) -> (Strict-Array A))))
(define (array-strict arr)
  (cond [(view-array? arr)
         (define ds (array-shape arr))
         (define g (unsafe-array-proc arr))
         (define size (array-size arr))
         (unsafe-strict-array ds (inline-build-array-data ds (λ (js j) (g js)) A))]
        [else  arr]))

(: array-lazy (All (A) ((Array A) -> (View-Array A))))
(define (array-lazy arr)
  (let ([arr  (array-view arr)])
    (define ds (array-shape arr))
    (define proc (unsafe-array-proc arr))
    (define: vs : (Vectorof (Promise A))
      (inline-build-array-data
       ds (λ (js j)
            ;; Because `delay' captures js in its closure, if we don't make this copy, `js' will have
            ;; been mutated by the time the promise is forced
            (let ([js  (vector-copy-all js)])
              (delay (proc js))))
       (Promise A)))
    (unsafe-view-array
     ds (λ: ([js : Indexes])
          (force (unsafe-vector-ref vs (unsafe-array-index->value-index ds js)))))))

;; ===================================================================================================
;; Printing

(require racket/pretty)

(: print-array-fields (All (A) ((Array A) Output-Port (U #t #f 0 1) -> Any)))
;; Mimicks the default custom printer for transparent struct values
(define (print-array-fields arr port mode)
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
  
  (cond [(view-array? arr)
         (write-string "(view-array" port)
         (maybe-print-newline 1)
         (recur-print (array-shape arr) port)
         (maybe-print-newline 1)
         (recur-print (unsafe-array-proc arr) port)
         (write-string ")" port)]
        [else
         (write-string "(strict-array" port)
         (maybe-print-newline 1)
         (recur-print (array-shape arr) port)
         (maybe-print-newline 1)
         (recur-print (strict-array-data arr) port)
         (write-string ")" port)]))

;; In "array.rkt", this is set to `print-array' from from "array-print.rkt"
(: array-custom-printer (Parameterof (All (A) ((Array A) Output-Port (U #t #f 0 1) -> Any))))
(define array-custom-printer (make-parameter print-array-fields))

;; ===================================================================================================
;; Comparison

(begin-encourage-inline
  
  (: view-array-compare (All (A) ((A A -> Boolean) (View-Array A) (View-Array A) Indexes -> Boolean)))
  ;; Assumes both arrays have shape `ds'
  (define (view-array-compare comp arr brr ds)
    (let/ec: return : Boolean
      (define f (unsafe-array-proc arr))
      (define g (unsafe-array-proc brr))
      (for-each-array-index ds (λ (js) (unless (comp (f js) (g js))
                                         (return #f))))
      #t))

  (: mixed-array-compare
     (All (A) ((A A -> Boolean) (View-Array A) (Strict-Array A) Indexes -> Boolean)))
  ;; Assumes both arrays have shape `ds'
  (define (mixed-array-compare comp arr brr ds)
    (let/ec: return : Boolean
      (define f (unsafe-array-proc arr))
      (define vs (strict-array-data brr))
      (for-each-array+data-index ds (λ (js j) (unless (comp (f js) (unsafe-vector-ref vs j))
                                                (return #f))))
      #t))

  (: strict-array-compare (All (A) ((A A -> Boolean) (Strict-Array A) (Strict-Array A) -> Boolean)))
  ;; Assumes arrays have the same size, and returns nonsense if they have different shapes
  (define (strict-array-compare comp arr brr)
    (define n (array-size arr))
    (define xs (strict-array-data arr))
    (define ys (strict-array-data brr))
    (let loop ([#{j : Nonnegative-Fixnum} 0])
      (cond [(j . < . n)
             (cond [(not (comp (unsafe-vector-ref xs j) (unsafe-vector-ref ys j)))  #f]
                   [else  (loop (+ j 1))])]
            [else  #t])))
  
  (: array-lift-comparison (All (A) ((A A -> Boolean) -> ((Array A) (Array A) -> Boolean))))
  (define ((array-lift-comparison comp) arr brr)
    (define ds (array-shape arr))
    (and (equal? ds (array-shape brr))
         (cond [(view-array? arr)
                (cond [(view-array? brr)  (view-array-compare comp arr brr ds)]
                      [else  (mixed-array-compare comp arr brr ds)])]
               [else
                (cond [(view-array? brr)  (mixed-array-compare (λ: ([x : A] [y : A]) (comp y x))
                                                               brr arr ds)]
                      [else  (strict-array-compare comp arr brr)])])))
  
  ) ; begin-encourage-inline
