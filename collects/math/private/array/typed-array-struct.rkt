#lang typed/racket/base

(require racket/promise
         racket/performance-hint
         "../unsafe.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide (all-defined-out))

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

(: array-guard (All (A) (Indexes Index Boolean (Indexes -> A) Symbol
                                 -> (Values Indexes Index Boolean (Indexes -> A)))))
(define (array-guard ds size strict? proc name)
  (cond [(zero? size)
         (let ([size  (array-shape-size ds)])
           (cond [(index? size)  (values (vector->immutable-vector ds) size strict? proc)]
                 [else  (error 'array "array size ~e (for shape ~e) is too large (is not an Index)"
                               size ds)]))]
        [else  (values (vector->immutable-vector ds) size strict? proc)]))

(struct: (A) Array ([shape : Indexes]
                    [size : Index]
                    [strict? : Boolean]
                    [unsafe-proc : (Indexes -> A)])
  #:guard array-guard
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (λ (arr port mode) ((array-custom-printer) arr 'array port mode))
  #:property prop:equal+hash (list array-recur-equal? array-hash-code array-hash-code)
  )

(define-syntax-rule (make-unsafe-array-proc ds ref)
  (λ: ([js : Indexes])
    (ref (unsafe-array-index->value-index ds js))))

(: array-dims (All (A) ((Array A) -> Index)))
(begin-encourage-inline
  (define (array-dims arr) (vector-length (Array-shape arr))))

(: build-array (All (A) (User-Indexes (Indexes -> A) -> (Array A))))
(define (build-array ds proc)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'build-array "(Vectorof Index)" 0 ds proc)))])
    (Array ds 0 #f (λ: ([js : Indexes])
                     (proc (vector->immutable-vector js))))))

(: unsafe-build-array (All (A) (Indexes (Indexes -> A) -> (Array A))))
(define (unsafe-build-array ds proc)
  (Array ds 0 #f proc))

(: flat-list->array (All (A) ((Vectorof Integer) (Listof A) -> (Array A))))
(define (flat-list->array ds lst)
  (let ([ds  (check-array-shape ds (λ () (raise-argument-error 'array "(Vectorof Index)" ds)))])
    (define vs (list->vector lst))
    (unsafe-build-array
     ds (λ: ([js : Indexes]) (unsafe-vector-ref vs (unsafe-array-index->value-index ds js))))))

(: array-lazy (All (A) ((Array A) -> (Array A))))
(define (array-lazy arr)
  (define ds (Array-shape arr))
  (define proc (Array-unsafe-proc arr))
  (define: vs : (Vectorof (Promise A))
    (inline-build-array-data
     ds (λ (js j)
          ;; Because `delay' captures js in its closure, if we don't make this copy, `js' will have
          ;; been mutated by the time the promise is forced
          (let ([js  (vector-copy-all js)])
            (delay (proc js))))
     (Promise A)))
  (unsafe-build-array
   ds (λ: ([js : Indexes])
        (force (unsafe-vector-ref vs (unsafe-array-index->value-index ds js))))))

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
  
  (write-string "[" port)
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
