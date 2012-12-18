#lang racket/base
(require racket/contract/base
         racket/vector
         racket/match)

(define MIN-SIZE 4)

(struct heap ([vec #:mutable] [count #:mutable] <=?))
;; length(vec)/4 <= size <= length(vec), except size >= MIN-SIZE
;; size = next available index

;; A VT is a binary tree represented as a vector.

;; VT Index functions

(define (vt-root) 0)

(define (vt-parent n) (quotient (sub1 n) 2))
(define (vt-leftchild n) (+ (* n 2) 1))
(define (vt-rightchild n) (+ (* n 2) 2))

(define (vt-root? n) (zero? n))
(define (vt-leftchild? n) (odd? n))
(define (vt-rightchild? n) (even? n))


;; Operations

(define (heapify-up <=? vec n)
  (unless (vt-root? n)
    (let* ([parent (vt-parent n)]
           [n-key (vector-ref vec n)]
           [parent-key (vector-ref vec parent)])
      (unless (<=? parent-key n-key)
        (vector-set! vec parent n-key)
        (vector-set! vec n parent-key)
        (heapify-up <=? vec parent)))))

(define (heapify-down <=? vec n size)
  (let ([left (vt-leftchild n)]
        [right (vt-rightchild n)]
        [n-key (vector-ref vec n)])
    (when (< left size)
      (let ([left-key (vector-ref vec left)])
        (let-values ([(child child-key)
                      (if (< right size)
                          (let ([right-key (vector-ref vec right)])
                            (if (<=? left-key right-key)
                                (values left left-key)
                                (values right right-key)))
                          (values left left-key))])
          (unless (<=? n-key child-key)
            (vector-set! vec n child-key)
            (vector-set! vec child n-key)
            (heapify-down <=? vec child size)))))))

(define (subheap? <=? vec n size)
  (let ([left (vt-leftchild n)]
        [right (vt-rightchild n)])
    (and (if (< left size)
             (<=? (vector-ref vec n) (vector-ref vec left))
             #t)
         (if (< right size)
             (<=? (vector-ref vec n) (vector-ref vec right))
             #t))))

(define (grow-vector v1 new-size-min)
  (let ([new-size (let loop ([size (vector-length v1)])
                    (if (>= size new-size-min)
                        size
                        (loop (* size 2))))])
    (let ([v2 (make-vector new-size #f)])
      (vector-copy! v2 0 v1 0)
      v2)))

(define (shrink-vector v1)
  (let ([v2 (make-vector (quotient (vector-length v1) 2) #f)])
    (vector-copy! v2 0 v1 0 (vector-length v2))
    v2))

;; Heaps

(define (make-heap <=?)
  (heap (make-vector MIN-SIZE #f) 0 <=?))

(define (list->heap <=? lst)
  (vector->heap <=? (list->vector lst)))

(define (vector->heap <=? vec0 [start 0] [end (vector-length vec0)])
  (define size (- end start))
  (define len (let loop ([len MIN-SIZE]) (if (<= size len) len (loop (* 2 len)))))
  (define vec (make-vector len #f))
  ;; size <= length(vec)
  (vector-copy! vec 0 vec0 start end)
  (for ([n (in-range (sub1 size) -1 -1)])
    (heapify-down <=? vec n size))
  (heap vec size <=?))

(define (heap-copy h)
  (match h
    [(heap vec count <=?)
     (heap (vector-copy vec) count <=?)]))

(define (heap-add! h . keys)
  (heap-add-all! h (list->vector keys)))

(define (heap-add-all! h keys)
  (let-values ([(keys keys-size)
                (cond [(list? keys)
                       (let ([keys-v (list->vector keys)])
                         (values keys-v (vector-length keys-v)))]
                      [(vector? keys)
                       (values keys (vector-length keys))]
                      [(heap? keys)
                       (values (heap-vec keys) (heap-count keys))])])
    (match h
      [(heap vec size <=?)
       (let* ([new-size (+ size keys-size)]
              [vec (if (> new-size (vector-length vec))
                       (let ([vec (grow-vector vec new-size)])
                         (set-heap-vec! h vec)
                         vec)
                       vec)])
         (vector-copy! vec size keys 0 keys-size)
         (for ([n (in-range size new-size)])
           (heapify-up <=? vec n))
         (set-heap-count! h new-size))])))

(define (heap-min h)
  (match h
    [(heap vec size <=?)
     (when (zero? size)
       (error 'heap-min "empty heap"))
     (vector-ref vec 0)]))

(define (heap-remove-min! h)
  (match h
    [(heap vec size <=?)
     (when (zero? size)
       (error 'heap-remove-min! "empty heap"))
     (heap-remove-index! h 0)]))

(define (heap-remove-index! h index)
  (match h
    [(heap vec size <=?)
     (unless (< index size)
       (if (zero? size)
           (error 'heap-remove-index!
                  "empty heap: ~s" index)
           (error 'heap-remove-index!
                  "index out of bounds [0,~s]: ~s" (sub1 size) index)))
     (vector-set! vec index (vector-ref vec (sub1 size)))
     (vector-set! vec (sub1 size) #f)
     (heapify-down <=? vec index (sub1 size))
     (when (< MIN-SIZE size (quotient (vector-length vec) 4))
       (set-heap-vec! h (shrink-vector vec)))
     (set-heap-count! h (sub1 size))]))

(define (in-heap h)
  (in-heap/consume! (heap-copy h)))

(define (in-heap/consume! h)
  (make-do-sequence
   (lambda ()
     (values (lambda (_) (heap-min h))
             (lambda (_) (heap-remove-min! h) #t)
             #t
             (lambda (_) (> (heap-count h) 0))
             (lambda _ #t)
             (lambda _ #t)))))

;; --------

;; preferred order is (heap-sort vec <=?), but allow old order too
(define (heap-sort! x y)
  (cond [(and (vector? x) (procedure? y))
         (heap-sort!* x y)]
        [(and (vector? y) (procedure? x))
         (heap-sort!* y x)]
        [else
         (unless (vector? x)
           (raise-argument-error 'heap-sort! "vector?" x))
         (raise-argument-error 'heap-sort! "procedure?" y)]))

(define (heap-sort!* v <=?)
  ;; to get ascending order, need max-heap, so reverse comparison
  (define (>=? x y) (<=? y x))
  (define size (vector-length v))
  (for ([n (in-range (sub1 size) -1 -1)])
    (heapify-down >=? v n size))
  (for ([last (in-range (sub1 size) 0 -1)])
    (let ([tmp (vector-ref v 0)])
      (vector-set! v 0 (vector-ref v last))
      (vector-set! v last tmp))
    (heapify-down >=? v 0 last)))

(define (heap->vector h)
  (match h
    [(heap vec size <=?)
     (let ([v (vector-copy vec 0 size)])
       (heap-sort!* v <=?)
       v)]))

;; --------

(provide/contract
 [make-heap (-> (-> any/c any/c any/c) heap?)]
 [heap? (-> any/c boolean?)]
 [heap-count (-> heap? exact-nonnegative-integer?)]
 [heap-add! (->* (heap?) () #:rest list? void?)]
 [heap-add-all! (-> heap? (or/c list? vector? heap?) void?)]
 [heap-min (-> heap? any/c)]
 [heap-remove-min! (-> heap? void?)]

 [vector->heap (-> (-> any/c any/c any/c) vector? heap?)]
 [heap->vector (-> heap? vector?)]
 [heap-copy (-> heap? heap?)]

 [in-heap (-> heap? sequence?)]
 [in-heap/consume! (-> heap? sequence?)])

(provide heap-sort!)
