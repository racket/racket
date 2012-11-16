#lang typed/racket/base

(require racket/performance-hint
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-indexing.rkt"
         "utils.rkt"
         "for-each.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Per-axis folds

(: check-array-axis (All (A) (Symbol (Array A) Integer -> Index)))
(define (check-array-axis name arr k)
  (define dims (array-dims arr))
  (cond
    [(= dims 0)  (raise-argument-error 'name "Array with at least one axis" 0 arr k)]
    [(or (0 . > . k) (k . >= . dims))
     (raise-argument-error 'name (format "Index < ~a" dims) 1 arr k)]
    [else  k]))

(: unsafe-array-axis-reduce (All (A B) ((Array A) Index (Index (Index -> A) -> B) -> (Array B))))
(begin-encourage-inline
  (define (unsafe-array-axis-reduce arr k f)
    (define ds (array-shape arr))
    (define dims (vector-length ds))
    (define dk (unsafe-vector-ref ds k))
    (define new-ds (unsafe-vector-remove ds k))
    (define proc (unsafe-array-proc arr))
    (unsafe-build-array
     new-ds (λ: ([js : Indexes])
              (define old-js (unsafe-vector-insert js k 0))
              (f dk (λ: ([jk : Index])
                      (unsafe-vector-set! old-js k jk)
                      (proc old-js)))))))

(: array-axis-fold/init (All (A B) ((Array A) Integer (A B -> B) B -> (Array B))))
(define (array-axis-fold/init arr k f init)
  (let ([k  (check-array-axis 'array-axis-fold arr k)])
    (unsafe-array-axis-reduce
     arr k (λ: ([dk : Index] [proc : (Index -> A)])
             (let: loop : B ([jk : Nonnegative-Fixnum  0] [acc : B  init])
               (cond [(jk . < . dk)  (loop (+ jk 1) (f (proc jk) acc))]
                     [else  acc]))))))

(: array-axis-fold/no-init (All (A) ((Array A) Integer (A A -> A) -> (Array A))))
(define (array-axis-fold/no-init arr k f)
  (let ([k  (check-array-axis 'array-axis-fold arr k)])
    (when (= (unsafe-vector-ref (array-shape arr) k) 0)
      (raise-argument-error 'array-axis-fold "nonzero axis" 0 arr k))
    (unsafe-array-axis-reduce
     arr k (λ: ([dk : Index] [proc : (Index -> A)])
             (let: loop : A ([jk : Nonnegative-Fixnum  1] [acc : A  (proc 0)])
               (cond [(jk . < . dk)  (loop (+ jk 1) (f (proc jk) acc))]
                     [else  acc]))))))

(: array-axis-fold (All (A B) (case-> ((Array A) Integer (A A -> A) -> (Array A))
                                      ((Array A) Integer (A B -> B) B -> (Array B)))))
(define array-axis-fold
  (case-lambda
    [(arr k f)  (array-axis-fold/no-init arr k f)]
    [(arr k f init)  (array-axis-fold/init arr k f init)]))

;; ---------------------------------------------------------------------------------------------------
;; Whole-array fold

(: array-fold (All (A) ((Array A) ((Array A) Index -> (Array A)) -> (Array A))))
(begin-encourage-inline
  (define (array-fold arr f)
    (define dims (array-dims arr))
    (let loop ([#{k : Index} dims] [arr arr])
      (cond [(zero? k)  arr]
            [else  (let ([k  (sub1 k)])
                     (loop k (f arr k)))]))))

;; ===================================================================================================
;; Standard axis folds

(define-syntax-rule (define-axis-fold name op T ...)
  (begin-encourage-inline
    (: name (case-> ((Array T) Integer -> (Array T)) ...
                    ((Array T) Integer T -> (Array T)) ...))
    (define name
      (case-lambda
        [(arr k)  (array-axis-fold arr k op)]
        [(arr k init)  (array-axis-fold arr k op init)]))))

(define-axis-fold array-axis-sum + Float Real Float-Complex Number)
(define-axis-fold array-axis-prod * Float Real Float-Complex Number)
(define-axis-fold array-axis-min min Float Real)
(define-axis-fold array-axis-max max Float Real)

;; ===================================================================================================
;; Standard whole-array folds

(define-syntax-rule (define-fold name array-axis-op T ...)
  (begin-encourage-inline
    (: name (case-> ((Array T) -> T) ...
                    ((Array T) T -> T) ...))
    (define name 
      (case-lambda
        [(arr)  (array-ref (array-fold arr array-axis-op) #())]
        [(arr init)
         (plet: (A) ([arr : (Array A)  arr]
                     [array-axis-op : ((Array A) Index A -> (Array A))  array-axis-op]
                     [init : A  init])
                (array-ref (array-fold arr (λ: ([arr : (Array A)] [k : Index])
                                             (array-axis-op arr k init)))
                           #()))]))))

(define-fold array-all-sum array-axis-sum Float Real Float-Complex Number)
(define-fold array-all-prod array-axis-prod Float Real Float-Complex Number)
(define-fold array-all-min array-axis-min Float Real)
(define-fold array-all-max array-axis-max Float Real)

;; ===================================================================================================
;; Count

(: array-axis-count (All (A) ((Array A) Integer (A -> Any) -> (Array Index))))
(define (array-axis-count arr k pred?)
  (let ([k  (check-array-axis 'array-axis-count arr k)])
    (unsafe-array-axis-reduce
     arr k (λ: ([dk : Index] [proc : (Index -> A)])
             (let: loop : Index ([jk : Nonnegative-Fixnum  0] [acc : Nonnegative-Fixnum  0])
               (if (jk . < . dk)
                   (cond [(pred? (proc jk))  (loop (+ jk 1) (unsafe-fx+ acc 1))]
                         [else  (loop (+ jk 1) acc)])
                   (assert acc index?)))))))

(: array-all-count (All (A) ((Array A) (A -> Any) -> Index)))
(define (array-all-count arr pred?)
  (define: i : (Boxof Nonnegative-Fixnum) (box 0))
  (define proc (unsafe-array-proc arr))
  (define ds (array-shape arr))
  (for-each-array-index ds (λ (js) (when (pred? (proc js)) (set-box! i (unsafe-fx+ (unbox i) 1)))))
  (assert (unbox i) index?))

;; ===================================================================================================
;; Short-cutting andmap

(: array-axis-andmap (All (A) ((Array A) Integer (A -> Any) -> (Array Boolean))))
(define (array-axis-andmap arr k pred?)
  (let ([k  (check-array-axis 'array-axis-andmap arr k)])
    (unsafe-array-axis-reduce
     arr k (λ: ([dk : Index] [proc : (Index -> A)])
             (let: loop : Boolean ([jk : Nonnegative-Fixnum  0])
               (cond [(jk . < . dk)  (if (pred? (proc jk)) (loop (+ jk 1)) #f)]
                     [else  #t]))))))

(: array-all-andmap (All (A) ((Array A) (A -> Any) -> Boolean)))
(define (array-all-andmap arr pred?)
  (let/ec: return : Boolean
    (define proc (unsafe-array-proc arr))
    (define ds (array-shape arr))
    (for-each-array-index ds (λ (js) (unless (pred? (proc js)) (return #f))))
    #t))

;; ===================================================================================================
;; Short-cutting ormap

(: array-axis-ormap (All (A) ((Array A) Integer (A -> Any) -> (Array Boolean))))
(define (array-axis-ormap arr k pred?)
  (let ([k  (check-array-axis 'array-axis-ormap arr k)])
    (unsafe-array-axis-reduce
     arr k (λ: ([dk : Index] [proc : (Index -> A)])
             (let: loop : Boolean ([jk : Nonnegative-Fixnum  0])
               (cond [(jk . < . dk)  (if (pred? (proc jk)) #t (loop (+ jk 1)))]
                     [else  #f]))))))

(: array-all-ormap (All (A) ((Array A) (A -> Any) -> Boolean)))
(define (array-all-ormap arr pred?)
  (let/ec: return : Boolean
    (define f (unsafe-array-proc arr))
    (define ds (array-shape arr))
    (for-each-array-index ds (λ (js) (when (pred? (f js)) (return #t))))
    #f))

;; ===================================================================================================

(: array-all-equal? ((Array Any) (Array Any) -> Boolean))
(define array-all-equal? equal?)

(: array-all-eqv? ((Array Any) (Array Any) -> Boolean))
(define array-all-eqv? (array-lift-comparison eqv?))

(: array-all-eq? ((Array Any) (Array Any) -> Boolean))
(define array-all-eq? (array-lift-comparison eq?))

(: array-all= ((Array Number) (Array Number) -> Boolean))
(define array-all= (array-lift-comparison =))
