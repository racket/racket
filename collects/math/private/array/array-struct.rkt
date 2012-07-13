#lang typed/racket/base

(require racket/unsafe/ops
         racket/performance-hint
         (for-syntax racket/base racket/syntax)
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
;; Equality

(: array-equal? (All (A) ((Array A) (Array A) (Any Any -> Boolean) -> Boolean)))
(define (array-equal? arr brr recur-equal?)
  (let ([arr  (array-strict arr)]
        [brr  (array-strict brr)])
    (and (equal? (unsafe-array-shape arr)
                 (unsafe-array-shape brr))
         (recur-equal? (unsafe-array-data arr)
                       (unsafe-array-data brr)))))

(: array-hash (All (A) ((Array A) (Any -> Integer) -> Integer)))
(define (array-hash arr recur-hash)
  (let ([arr  (array-strict arr)])
    (bitwise-xor (recur-hash (unsafe-array-shape arr))
                 (recur-hash (unsafe-array-data arr)))))

;; ===================================================================================================
;; Parent array data type

(: array-guard ((Vectorof Index) Symbol -> (Vectorof Index)))
(define (array-guard ds name)
  (define size (unsafe-array-shape-size ds))
  (cond [(index? size)  ds]
        [else  (error name "array size ~e (shape ~e) is not an Index" size ds)]))

(struct: (A) array ([shape : (Vectorof Index)])
  #:property prop:equal+hash (list array-equal? array-hash array-hash)
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
;; Lazy conversion

(: array-lazy (All (A) ((Array A) -> (lazy-array A))))
(begin-encourage-inline
  (define (array-lazy arr)
    (cond [(lazy-array? arr)  arr]
          [else  (define ds (unsafe-array-shape arr))
                 (define vs (unsafe-array-data arr))
                 (unsafe-lazy-array
                  ds (λ: ([js : (Vectorof Index)])
                       (unsafe-vector-ref vs (unsafe-array-index->value-index ds js))))])))

;; ===================================================================================================
;; Strict conversion

(: array-strict (All (A) ((Array A) -> (strict-array A))))
(begin-encourage-inline
  (define (array-strict arr)
    (cond
      [(lazy-array? arr)
       (define ds (unsafe-array-shape arr))
       (define proc (unsafe-array-proc arr))
       (define size (array-size arr))
       (define dims (vector-length ds))
       (define: vs : (Vectorof A)
         (if (= size 0)
             (vector)
             (case dims
               [(0)  (vector (proc ds))]
               [(1)  (make-strict-data-1 ds proc size)]
               [(2)  (make-strict-data-2 ds proc size)]
               [(3)  (make-strict-data-3 ds proc size)]
               [(4)  (make-strict-data-4 ds proc size)]
               [(5)  (make-strict-data-5 ds proc size)]
               [else  (make-strict-data ds proc size dims)])))
       (unsafe-strict-array ds vs)]
      [else  arr])))

(: make-strict-data (All (A) ((Vectorof Index) ((Vectorof Index) -> A) Fixnum Index -> (Vectorof A))))
(define (make-strict-data ds proc size dims)
  (define: js : (Vectorof Index) (make-vector dims 0))
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
  vs)

(: make-strict-data-1 (All (A) ((Vectorof Index) ((Vectorof Index) -> A) Fixnum -> (Vectorof A))))
(define (make-strict-data-1 ds proc size)
  (define d0 (unsafe-vector-ref ds 0))
  (define: js : (Vectorof Index) (make-vector 1 0))
  (define vs (make-vector size (proc js)))
  (let j0-loop ([#{j0 : Nonnegative-Fixnum}  0])
    (cond [(j0 . < . d0)
           (unsafe-vector-set! js 0 j0)
           (unsafe-vector-set! vs j0 (proc js))
           (j0-loop (+ j0 1))]
          [else  vs])))

(define-syntax (define-make-strict-data-n stx)
  (syntax-case stx ()
    [(the-name dims)
     (let ([n  (syntax->datum #'dims)])
       (with-syntax*
           ([name  (format-id #'the-name "make-strict-data-~a" n)]
            [i #'j]
            [(k ...)  (build-list n values)]
            [(d ...)  (generate-temporaries (build-list n (λ (k) (format-id #'i "d~a" k))))]
            [(j ...)  (generate-temporaries (build-list n (λ (k) (format-id #'i "j~a" k))))]
            [(loop ...)  (generate-temporaries (build-list n (λ (k) (format-id #'i "j~a-loop" k))))])
         (syntax/loc stx
           (begin
             (: name (All (A) ((Vectorof Index) ((Vectorof Index) -> A) Fixnum -> (Vectorof A))))
             (define (name ds proc size)
               (define d (unsafe-vector-ref ds k)) ...
               (define i 0)
               (define: js : (Vectorof Index) (make-vector dims 0))
               (define vs (make-vector size (proc js)))
               (make-strict-data-n A i js proc vs (k ...) (d ...) (loop ...) (j ...) vs))))))]))

(define-syntax (make-strict-data-n stx)
  (syntax-case stx ()
    [(_ A i js proc vs (k0) (dk) (jk-loop) (jk) next)
     (syntax/loc stx
       (let: jk-loop : (Vectorof A) ([jk : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
         (cond [(jk . < . dk)
                (unsafe-vector-set! js k0 jk)
                (unsafe-vector-set! vs i (proc js))
                (jk-loop (+ jk 1) (unsafe-fx+ i 1))]
               [else  next])))]
    [(_ A i js proc vs (k0 k ...) (dk d ...) (jk-loop loop ...) (jk j ...) next)
     (syntax/loc stx
       (let: jk-loop : (Vectorof A) ([jk : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
         (cond [(jk . < . dk)
                (unsafe-vector-set! js k0 jk)
                (make-strict-data-n A i js proc vs (k ...) (d ...) (loop ...) (j ...)
                                    (jk-loop (+ jk 1) i))]
               [else  next])))]))

(: make-strict-data-2 (All (A) ((Vectorof Index) ((Vectorof Index) -> A) Fixnum -> (Vectorof A))))
(define-make-strict-data-n 2)

(: make-strict-data-3 (All (A) ((Vectorof Index) ((Vectorof Index) -> A) Fixnum -> (Vectorof A))))
(define-make-strict-data-n 3)

(: make-strict-data-4 (All (A) ((Vectorof Index) ((Vectorof Index) -> A) Fixnum -> (Vectorof A))))
(define-make-strict-data-n 4)

(: make-strict-data-5 (All (A) ((Vectorof Index) ((Vectorof Index) -> A) Fixnum -> (Vectorof A))))
(define-make-strict-data-n 5)

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
