#lang typed/racket/base

(require racket/performance-hint
         racket/fixnum
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-indexing.rkt"
         "utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Per-axis folds

(: check-array-axis (All (A) (Symbol (Array A) Integer -> Index)))
(define (check-array-axis name arr k)
  (define dims (array-dims arr))
  (cond
    [(fx= dims 0)  (raise-argument-error name "Array with at least one axis" 0 arr k)]
    [(or (0 . > . k) (k . >= . dims))
     (raise-argument-error name (format "Index < ~a" dims) 1 arr k)]
    [else  k]))

(: unsafe-array-axis-reduce (All (A B) ((Array A) Index (Index (Index -> A) -> B) -> (Array B))))
(begin-encourage-inline
  (define (unsafe-array-axis-reduce arr k f)
    (define ds (array-shape arr))
    (define dk (unsafe-vector-ref ds k))
    (define new-ds (unsafe-vector-remove ds k))
    (define proc (unsafe-array-proc arr))
    (unsafe-build-array
     new-ds (λ: ([js : Indexes])
              (define old-js (unsafe-vector-insert js k 0))
              (f dk (λ: ([jk : Index])
                      (unsafe-vector-set! old-js k jk)
                      (proc old-js)))))))

(: array-axis-reduce (All (A B) ((Array A) Integer (Index (Integer -> A) -> B) -> (Array B))))
(define (array-axis-reduce arr k f)
  (let ([k  (check-array-axis 'array-axis-reduce arr k)])
    (array-default-strict
     (unsafe-array-axis-reduce
      arr k
      (λ: ([dk : Index] [proc : (Index -> A)])
        (: safe-proc (Integer -> A))
        (define (safe-proc jk)
          (cond [(or (jk . < . 0) (jk . >= . dk))
                 (raise-argument-error 'array-axis-reduce (format "Index < ~a" dk) jk)]
                [else  (proc jk)]))
        (f dk safe-proc))))))

(: array-axis-fold/init (All (A B) ((Array A) Integer (A B -> B) B -> (Array B))))
(define (array-axis-fold/init arr k f init)
  (let ([k  (check-array-axis 'array-axis-fold arr k)])
    (unsafe-array-axis-reduce
     arr k (λ: ([dk : Index] [proc : (Index -> A)])
             (let: loop : B ([jk : Nonnegative-Fixnum  0] [acc : B  init])
               (cond [(jk . fx< . dk)  (loop (fx+ jk 1) (f (proc jk) acc))]
                     [else  acc]))))))

(: array-axis-fold/no-init (All (A) ((Array A) Integer (A A -> A) -> (Array A))))
(define (array-axis-fold/no-init arr k f)
  (let ([k  (check-array-axis 'array-axis-fold arr k)])
    (when (fx= (unsafe-vector-ref (array-shape arr) k) 0)
      (raise-argument-error 'array-axis-fold "nonzero axis" 0 arr k))
    (unsafe-array-axis-reduce
     arr k (λ: ([dk : Index] [proc : (Index -> A)])
             (let: loop : A ([jk : Nonnegative-Fixnum  1] [acc : A  (proc 0)])
               (cond [(jk . fx< . dk)  (loop (fx+ jk 1) (f (proc jk) acc))]
                     [else  acc]))))))

(: array-axis-fold (All (A B) (case-> ((Array A) Integer (A A -> A) -> (Array A))
                                      ((Array A) Integer (A B -> B) B -> (Array B)))))
(define array-axis-fold
  (case-lambda
    [(arr k f)  (array-default-strict (array-axis-fold/no-init arr k f))]
    [(arr k f init)  (array-default-strict (array-axis-fold/init arr k f init))]))

;; ===================================================================================================
;; Whole-array folds

(begin-encourage-inline
  
  (: array-fold (All (A) ((Array A) ((Array A) Index -> (Array A)) -> (Array A))))
  (define (array-fold arr f)
    (define dims (array-dims arr))
    (let loop ([#{k : Index} dims] [arr arr])
      (cond [(fx= k 0)  arr]
            [else  (let ([k  (fx- k 1)])
                     (loop k (f arr k)))])))
  
  (: array-all-fold (All (A) (case-> ((Array A) (A A -> A) -> A)
                                     ((Array A) (A A -> A) A -> A))))
  (define array-all-fold
    (case-lambda
      [(arr f)
       ;; Though `f' is folded over multiple axes, each element of `arr' is referred to only once, so
       ;; turning strictness off can't hurt performance
       (parameterize ([array-strictness #f])
         (array-ref (array-fold arr (λ: ([arr : (Array A)] [k : Index])
                                      (array-axis-fold arr k f)))
                    #()))]
      [(arr f init)
       ;; See above for why non-strictness is okay
       (parameterize ([array-strictness #f])
         (array-ref (array-fold arr (λ: ([arr : (Array A)] [k : Index])
                                      (array-axis-fold arr k f init)))
                    #()))]))
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; Count

(: array-axis-count (All (A) ((Array A) Integer (A -> Any) -> (Array Index))))
(define (array-axis-count arr k pred?)
  (let ([k  (check-array-axis 'array-axis-count arr k)])
    (array-default-strict
     (unsafe-array-axis-reduce
      arr k (λ: ([dk : Index] [proc : (Index -> A)])
              (let: loop : Index ([jk : Nonnegative-Fixnum  0] [acc : Nonnegative-Fixnum  0])
                (if (jk . fx< . dk)
                    (cond [(pred? (proc jk))  (loop (fx+ jk 1) (unsafe-fx+ acc 1))]
                          [else  (loop (fx+ jk 1) acc)])
                    (assert acc index?))))))))

;; ===================================================================================================
;; Short-cutting axis folds

(: array-axis-and (All (A) ((Array A) Integer -> (Array (U A Boolean)))))
(define (array-axis-and arr k)
  (let ([k  (check-array-axis 'array-axis-and arr k)])
    (array-default-strict
     (unsafe-array-axis-reduce
      arr k (λ: ([dk : Index] [proc : (Index -> A)])
              (let: loop : (U A Boolean) ([jk : Nonnegative-Fixnum  0] [acc : (U A Boolean)  #t])
                (cond [(jk . fx< . dk)  (define v (and acc (proc jk)))
                                        (if v (loop (fx+ jk 1) v) v)]
                      [else  acc])))))))

(: array-axis-or (All (A) ((Array A) Integer -> (Array (U A #f)))))
(define (array-axis-or arr k)
  (let ([k  (check-array-axis 'array-axis-or arr k)])
    (array-default-strict
     (unsafe-array-axis-reduce
      arr k (λ: ([dk : Index] [proc : (Index -> A)])
              (let: loop : (U A #f) ([jk : Nonnegative-Fixnum  0] [acc : (U A #f)  #f])
                (cond [(jk . fx< . dk)  (define v (or acc (proc jk)))
                                        (if v v (loop (fx+ jk 1) v))]
                      [else  acc])))))))

(: array-all-and (All (A B) ((Array A) -> (U A Boolean))))
(define (array-all-and arr)
  ;; See `array-all-fold' for why non-strictness is okay
  (parameterize ([array-strictness #f])
    (array-ref ((inst array-fold (U A Boolean)) arr array-axis-and) #())))

(: array-all-or (All (A B) ((Array A) -> (U A #f))))
(define (array-all-or arr)
  ;; See `array-all-fold' for why non-strictness is okay
  (parameterize ([array-strictness #f])
    (array-ref ((inst array-fold (U A #f)) arr array-axis-or) #())))

;; ===================================================================================================
;; Other folds

(: array->list-array (All (A) (case-> ((Array A) -> (Array (Listof A)))
                                      ((Array A) Integer -> (Array (Listof A))))))
(define (array->list-array arr [k 0])
  (define dims (array-dims arr))
  (cond [(and (k . >= . 0) (k . < . dims))
         (array-default-strict
          (unsafe-array-axis-reduce arr k (inst build-list A)))]
        [else
         (raise-argument-error 'array->list-array (format "Index < ~a" dims) 1 arr k)]))
