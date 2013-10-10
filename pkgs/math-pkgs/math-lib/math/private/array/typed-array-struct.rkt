#lang typed/racket/base

(require racket/promise
         racket/performance-hint
         "../unsafe.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide (all-defined-out))

(: array-strictness (Parameterof (U #f #t)))
(define array-strictness (make-parameter #t))

;; ===================================================================================================
;; Equality and hashing

(: array-lift-comparison (All (A) ((A A -> Boolean) -> ((Array A) (Array A) -> Boolean))))
(define ((array-lift-comparison comp) arr brr)
  (define ds (Array-shape arr))
  (and (equal? ds (Array-shape brr))
       (let/ec: return : Boolean
         (define f (Array-unsafe-proc arr))
         (define g (Array-unsafe-proc brr))
         (for-each-array-index ds (λ (js) (unless (comp (f js) (g js))
                                            (return #f))))
         #t)))

(: array-recur-equal? ((Array Any) (Array Any) (Any Any -> Boolean) -> Boolean))
(define (array-recur-equal? arr brr recur-equal?)
  ((array-lift-comparison recur-equal?) arr brr))

(: array-hash-code (All (A) ((Array A) (Any -> Integer) -> Integer)))
(define (array-hash-code arr recur-hash-code)
  (define ds (Array-shape arr))
  (define f (Array-unsafe-proc arr))
  (define h 0)
  (for-each-array-index ds (λ (js) (set! h (bitwise-xor h (recur-hash-code (f js))))))
  (bitwise-xor h (recur-hash-code ds)))

;; ===================================================================================================
;; Array data type: a function whose domain has a rectangular shape

#|
(: array-procedure (All (A) ((Array A) In-Indexes -> A)))
(define (array-procedure arr js)
  ((Array-unsafe-proc arr) (check-array-indexes 'array-ref (Array-shape arr) js)))
|#

(struct: (A) Array ([shape : Indexes]
                    [size : Index]
                    [strict? : (Boxof Boolean)]
                    [strict! : (-> Void)]
                    [unsafe-proc : (Indexes -> A)])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (λ (arr port mode) ((array-custom-printer) arr 'array port mode))
  #:property prop:equal+hash (list array-recur-equal? array-hash-code array-hash-code)
  ;; It would be really nice to do this, but TR can't right now:
  ;#:property prop:procedure array-procedure
  )

(define-syntax-rule (make-unsafe-array-proc ds ref)
  (λ: ([js : Indexes])
    (ref (unsafe-array-index->value-index ds js))))

(: array-dims (All (A) ((Array A) -> Index)))
(define (array-dims arr)
  (vector-length (Array-shape arr)))

(: array-strict? (All (A) ((Array A) -> Boolean)))
(define (array-strict? arr)
  (unbox (Array-strict? arr)))

(: array-strict! (All (A) ((Array A) -> Void)))
(define (array-strict! arr)
  (define strict? (Array-strict? arr))
  (unless (unbox strict?)
    ((Array-strict! arr))
    (set-box! strict? #t)))

(: array-default-strict! (All (A) ((Array A) -> Void)))
(define (array-default-strict! arr)
  (define strict? (Array-strict? arr))
  (when (and (not (unbox strict?)) (array-strictness))
    ((Array-strict! arr))
    (set-box! strict? #t)))

(: unsafe-build-array (All (A) (Indexes (Indexes -> A) -> (Array A))))
(define (unsafe-build-array ds f)
  ;; This box's contents get replaced when the array we're constructing is made strict, so that
  ;; the array stops referencing f. If we didn't do this, long chains of array computations would
  ;; keep hold of references to all the intermediate procs, which is a memory leak.
  (let ([f  (box f)])
    (define size (check-array-shape-size 'unsafe-build-array ds))
    ;; Sharp readers might notice that strict! doesn't check to see whether the array is already
    ;; strict; that's okay - array-strict! does it instead, which makes the "once strict, always
    ;; strict" invariant easier to ensure in subtypes, which we don't always have control over
    (define (strict!)
      (let* ([old-f  (unbox f)]
             [vs     (inline-build-array-data ds (λ (js j) (old-f js)) A)])
        ;; Make a new f that just indexes into vs
        (set-box! f (λ: ([js : Indexes])
                      (unsafe-vector-ref vs (unsafe-array-index->value-index ds js))))))
    (define unsafe-proc
      (λ: ([js : Indexes]) ((unbox f) js)))
    (Array ds size ((inst box Boolean) #f) strict! unsafe-proc)))

(: unsafe-build-simple-array (All (A) (Indexes (Indexes -> A) -> (Array A))))
(define (unsafe-build-simple-array ds f)
  (define size (check-array-shape-size 'unsafe-build-simple-array ds))
  (Array ds size (box #t) void f))

(: build-array (All (A) (In-Indexes (Indexes -> A) -> (Array A))))
(define (build-array ds proc)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'build-array "(Vectorof Index)" 0 ds proc)))])
    (define arr
      (unsafe-build-array ds (λ: ([js : Indexes])
                               (proc (vector->immutable-vector js)))))
    (array-default-strict! arr)
    arr))

(: build-simple-array (All (A) (In-Indexes (Indexes -> A) -> (Array A))))
(define (build-simple-array ds proc)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'build-simple-array "(Vectorof Index)" 0 ds proc)))])
    (unsafe-build-simple-array ds (λ: ([js : Indexes])
                                    (proc (vector->immutable-vector js))))))

(: unsafe-list->array (All (A) (Indexes (Listof A) -> (Array A))))
(define (unsafe-list->array ds xs)
  (define vs (list->vector xs))
  (unsafe-build-simple-array
   ds (λ: ([js : Indexes]) (unsafe-vector-ref vs (unsafe-array-index->value-index ds js)))))

(: list->array (All (A) (case-> ((Listof A) -> (Array A))
                                (In-Indexes (Listof A) -> (Array A)))))
(define list->array
  (case-lambda
    [(xs)  (unsafe-list->array ((inst vector Index) (length xs)) xs)]
    [(ds xs)
     (let* ([ds  (check-array-shape
                  ds (λ () (raise-argument-error 'list->array "(Vectorof Index)" 0 ds xs)))]
            [size  (array-shape-size ds)]
            [n  (length xs)])
       (if (= size n)
           (unsafe-list->array ds xs)
           (raise-argument-error 'list->array (format "List of length ~e" size) 1 ds xs)))]))

(: array-lazy (All (A) ((Array A) -> (Array A))))
(define (array-lazy arr)
  (define ds (Array-shape arr))
  (define size (Array-size arr))
  (define proc (Array-unsafe-proc arr))
  (define: vs : (Vectorof (Promise A))
    (inline-build-array-data
     ds (λ (js j)
          ;; Because `delay' captures js in its closure, if we don't make this copy, `js' will have
          ;; been mutated by the time the promise is forced
          ;; Using vector->immutable-vector to copy won't work: `proc' might need to mutate `js'
          (let ([js  (vector-copy-all js)])
            (delay (proc js))))
     (Promise A)))
  (define (strict!) (for: ([v  (in-vector vs)]) (force v)))
  (define unsafe-proc
    (λ: ([js : Indexes])
      (force (unsafe-vector-ref vs (unsafe-array-index->value-index ds js)))))
  (Array ds size ((inst box Boolean) #f) strict! unsafe-proc))

;; ===================================================================================================
;; Abstract settable array data type

(struct: (A) Settable-Array Array ([set-proc : (Indexes A -> Void)]))

(define settable-array? Settable-Array?)
(define unsafe-settable-array-set-proc Settable-Array-set-proc)

(define-syntax-rule (make-unsafe-array-set-proc A ds set!)
  (λ: ([js : Indexes] [v : A])
    (set! (unsafe-array-index->value-index ds js) v)))

;; ===================================================================================================
;; Printing

(require racket/pretty)

(: print-array-fields (All (A) ((Array A) Symbol Output-Port (U #t #f 0 1) -> Any)))
;; Mimicks the default custom printer for transparent struct values
(define (print-array-fields arr name port mode)
  (define ds (Array-shape arr))
  (define proc (Array-unsafe-proc arr))
  
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
  
  (write-string (format "#<~a" name) port)
  (maybe-print-newline 2)
  (recur-print ds port)
  (maybe-print-newline 2)
  
  (define lst null)
  (for-each-array-index ds (λ (js) (set! lst (cons (proc js) lst))))
  
  (write-string "#[" port)
  (unless (null? lst)
    (let ([lst  (reverse lst)])
      (recur-print (car lst) port)
      (for ([x  (in-list (cdr lst))])
        (maybe-print-newline 3)
        (recur-print x port))))
  (write-string "]>" port))

;; In math/array, this is set to `print-array' from "array-print.rkt"
(: array-custom-printer (Parameterof (All (A) ((Array A) Symbol Output-Port (U #t #f 0 1) -> Any))))
(define array-custom-printer (make-parameter print-array-fields))
