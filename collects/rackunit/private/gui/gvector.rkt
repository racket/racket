#lang racket/base
(require racket/contract
         racket/dict)

(define (make-gvector* #:capacity [capacity 10])
  (make-gvector (make-vector capacity #f) 0))

(define (check-index who index n)
  (unless (exact-nonnegative-integer? index)
    (raise-type-error who "exact nonnegative integer" index))
  (unless (< index n)
    (if (zero? n)
        (error who "index out of range for empty gvector: ~s" index)
        (error who "index out of range [0,~s]: ~s" (sub1 n) index))))

(define ((bad-index-error who index))
  (raise-mismatch-error who "index out of range" index))

(define (gvector-add! gv item)
  (let ([n (gvector-n gv)]
        [v (gvector-vec gv)])
    (cond [(< n (vector-length v))
           (vector-set! v n item)
           (set-gvector-n! gv (add1 n))]
          [else
           (let ([nv (make-vector (* 2 n) #f)])
             (vector-copy! nv 0 v)
             (vector-set! nv n item)
             (set-gvector-vec! gv nv)
             (set-gvector-n! gv (add1 n)))])))

;; SLOW!
(define (gvector-remove! gv index)
  (let ([n (gvector-n gv)]
        [v (gvector-vec gv)])
    (check-index 'gvector-remove! index n)
    (set-gvector-n! gv (sub1 n))
    (vector-copy! v index v (add1 index) n)
    (vector-set! v (sub1 n) #f)))

(define (gvector-count gv)
  (gvector-n gv))

(define (gvector-ref gv index
                     [default (bad-index-error 'gvector-ref index)])
  (unless (exact-nonnegative-integer? index)
    (raise-type-error 'gvector-ref "exact nonnegative integer" index))
  (if (< index (gvector-n gv))
      (vector-ref (gvector-vec gv) index)
      (if (procedure? default)
          (default)
          default)))

(define (gvector-set! gv index item)
  (check-index 'gvector-set! index (gvector-n gv))
  (vector-set! (gvector-vec gv) index item))

;; Iteration methods

(define (gvector-iterate-first gv)
  (and (positive? (gvector-n gv)) 0))

(define (gvector-iterate-next gv iter)
  (check-index 'gvector-iterate-next iter (gvector-n gv))
  (let ([n (gvector-n gv)])
    (and (< (add1 iter) n)
         (add1 iter))))

(define (gvector-iterate-key gv iter)
  (check-index 'gvector-iterate-key iter (gvector-n gv))
  iter)

(define (gvector-iterate-value gv iter)
  (check-index 'gvector-iterate-value iter (gvector-n gv))
  (gvector-ref gv iter))

(define (in-gvector gv)
  (unless (gvector? gv)
    (raise-type-error 'in-gvector "gvector" gv))
  (in-dict-values gv))

(define-struct gvector (vec n)
  #:mutable
  #:property prop:dict
             (vector gvector-ref
                     gvector-set!
                     #f ;; set
                     gvector-remove!
                     #f ;; remove
                     gvector-count
                     gvector-iterate-first
                     gvector-iterate-next
                     gvector-iterate-key
                     gvector-iterate-value)
  #:property prop:sequence in-gvector)

(provide/contract
 [rename make-gvector* make-gvector
  (->* () (#:capacity exact-positive-integer?) any)]
 [gvector-ref
  (-> gvector? exact-nonnegative-integer? any)]
 [gvector-set!
  (-> gvector? exact-nonnegative-integer? any/c any)]
 [gvector-add!
  (-> gvector? any/c any)]
 [gvector-remove!
  (-> gvector? exact-nonnegative-integer? any)]
 [gvector-count
  (-> gvector? any)]
 [in-gvector
  (-> gvector? sequence?)])
