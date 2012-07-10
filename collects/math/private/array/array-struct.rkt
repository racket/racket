#lang typed/racket/base

(require racket/unsafe/ops
         racket/performance-hint
         "utils.rkt")

(define-type (Array A) (U (lazy-array A) (strict-array A)))
(define-type (-lazy-array A) (lazy-array A))
(define-type (-strict-array A) (strict-array A))

;; Error: result with wrong filter
#;;(: -array? (Any -> Boolean : (Array Any)))
(define (-array? v)
  (or (lazy-array? v) (strict-array? v)))

#;; Error: can't convert to a contract
(define-predicate -array? (Array Any))

(provide Array (rename-out ;[-array?        array?]
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
;; Parent array data type

(struct: array ([shape : (Vectorof Index)]))

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
  
  (: safe-array-shape (All (A) ((Array A) -> (Listof Integer))))
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
              ds (λ () (raise-type-error 'lazy-array "(Listof Index) with Index product"
                                         0 ds proc)))])
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
              ds (λ () (raise-type-error 'strict-array "(Listof Index) with Index product"
                                         0 ds vs)))])
    (define size (unsafe-array-shape-size ds))
    (unless (= size (vector-length vs))
      (raise-type-error 'strict-array (format "Vector of length ~e" size)
                        1 (array-shape-unsafe->safe ds) vs))
    (strict-array ds vs)))

(: unsafe-strict-array (All (A) ((Vectorof Index) (Vectorof A) -> (strict-array A))))
(define (unsafe-strict-array ds vs)
  (strict-array ds vs))

(define unsafe-array-data strict-array-data)

;; ===================================================================================================
;; Lazy conversion

(begin-encourage-inline
  
  (: array-lazy (All (A) ((Array A) -> (lazy-array A))))
  (define (array-lazy arr)
    (cond [(lazy-array? arr)  arr]
          [else  (define ds (unsafe-array-shape arr))
                 (define vs (unsafe-array-data arr))
                 (unsafe-lazy-array
                  ds (λ: ([js : (Vectorof Index)])
                       (unsafe-vector-ref vs (unsafe-array-index->value-index ds js))))]))
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; Strict conversion

(: array-strict (All (A) ((Array A) -> (strict-array A))))
(define (array-strict arr)
  (cond
    [(lazy-array? arr)
     (define ds (unsafe-array-shape arr))
     (define proc (unsafe-array-proc arr))
     (define size (unsafe-array-shape-size ds))
     (define dims (vector-length ds))
     (define: vs : (Vectorof A)
       (cond [(= size 0)  (vector)]
             [else
              (case dims
                [(0)  (vector (proc ds))]
                [(1)  (define d0 (unsafe-vector-ref ds 0))
                      (define: js : (Vectorof Index) (make-vector dims 0))
                      (define vs (make-vector size (proc js)))
                      (let j0-loop ([#{j0 : Nonnegative-Fixnum}  0])
                        (cond [(j0 . < . d0)
                               (unsafe-vector-set! js 0 j0)
                               (unsafe-vector-set! vs j0 (proc js))
                               (j0-loop (+ j0 1))]
                              [else  vs]))]
                [(2)  (define d0 (unsafe-vector-ref ds 0))
                      (define d1 (unsafe-vector-ref ds 1))
                      (define: js : (Vectorof Index) (make-vector dims 0))
                      (define vs (make-vector size (proc js)))
                      (let j0-loop ([#{j0 : Nonnegative-Fixnum} 0]
                                    [#{j : Nonnegative-Fixnum} 0])
                        (cond [(j0 . < . d0)
                               (unsafe-vector-set! js 0 j0)
                               (let j1-loop ([#{j1 : Nonnegative-Fixnum} 0]
                                             [#{j : Nonnegative-Fixnum} j])
                                 (cond [(j1 . < . d1)
                                        (unsafe-vector-set! js 1 j1)
                                        (unsafe-vector-set! vs j (proc js))
                                        (j1-loop (+ j1 1) (unsafe-fx+ j 1))]
                                       [else
                                        (j0-loop (+ j0 1) j)]))]
                              [else  vs]))]
                [(3)  (define d0 (unsafe-vector-ref ds 0))
                      (define d1 (unsafe-vector-ref ds 1))
                      (define d2 (unsafe-vector-ref ds 2))
                      (define: js : (Vectorof Index) (make-vector dims 0))
                      (define vs (make-vector size (proc js)))
                      (let j0-loop ([#{j0 : Nonnegative-Fixnum} 0]
                                    [#{j : Nonnegative-Fixnum} 0])
                        (cond
                          [(j0 . < . d0)
                           (unsafe-vector-set! js 0 j0)
                           (let j1-loop ([#{j1 : Nonnegative-Fixnum} 0]
                                         [#{j : Nonnegative-Fixnum} j])
                             (cond
                               [(j1 . < . d1)
                                (unsafe-vector-set! js 1 j1)
                                (let j2-loop ([#{j2 : Nonnegative-Fixnum} 0]
                                              [#{j : Nonnegative-Fixnum} j])
                                  (cond
                                    [(j2 . < . d2)
                                     (unsafe-vector-set! js 2 j2)
                                     (unsafe-vector-set! vs j (proc js))
                                     (j2-loop (+ j2 1) (unsafe-fx+ j 1))]
                                    [else
                                     (j1-loop (+ j1 1) j)]))]
                               [else
                                (j0-loop (+ j0 1) j)]))]
                          [else  vs]))]
                ;; General case
                [else  (define: js : (Vectorof Index) (make-vector dims 0))
                       (define vs (make-vector size (proc js)))
                       (let: i-loop : Nonnegative-Fixnum ([i : Nonnegative-Fixnum  0]
                                                          [j : Nonnegative-Fixnum  0])
                         (cond [(i . < . dims)
                                (define di (unsafe-vector-ref ds i))
                                (let: ji-loop : Nonnegative-Fixnum ([ji : Nonnegative-Fixnum  0]
                                                                    [j : Nonnegative-Fixnum  j])
                                  (cond [(ji . < . di)
                                         (unsafe-vector-set! js i ji)
                                         (ji-loop (+ ji 1) (i-loop (+ i 1) j))]
                                        [else  j]))]
                               [else
                                (unsafe-vector-set! vs j (proc js))
                                (unsafe-fx+ j 1)]))
                       vs])]))
     (unsafe-strict-array ds vs)]
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
