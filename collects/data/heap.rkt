#lang racket/base
(require racket/vector
         racket/match)

(define MIN-SIZE 4)

(define-struct heap (vec size <=?) #:mutable)
;; length(vec)/4 <= size <= length(vec)
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
        (heapify-up vec parent)))))

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
            (heapify-down vec child size)))))))

(define (subheap? <=? vec n size)
  (let ([left (vt-leftchild n)]
        [right (vt-rightchild n)])
    (and (if (< left size)
             (<=? (vector-ref vec n) (vector-ref vec left))
             #t)
         (if (< right size)
             (<=? (vector-ref vec n) (vector-ref vec right))
             #t))))

(define (grow-vector v1)
  (let ([v2 (make-vector (* (vector-length v1) 2) #f)])
    (vector-copy! v2 0 v1 0)
    v2))

(define (shrink-vector v1)
  (let ([v2 (make-vector (quotient (vector-length v1) 2) #f)])
    (vector-copy! v2 0 v1 0 (vector-length v2))
    v2))

;; Heaps

(define make-heap*
  (let ([make-heap
         (lambda (<=?) (make-heap (make-vector MIN-SIZE #f) 0 <=?))])
    make-heap))

(define (vector->heap <=? vec0 [start 0] [end (vector-length vec0)])
  (define size (- end start))
  (define len (let loop ([len MIN-SIZE]) (if (<= size len) len (loop (* 2 len)))))
  (define vec (make-vector len #f))
  ;; size <= length(vec)
  (vector-copy! vec 0 vec0 start end)
  (for ([n (in-range (sub1 size) -1 -1)])
    (heapify-down <=? vec n size))
  (make-heap vec size <=?))

(define (heap-copy h)
  (match h
    [(heap vec count <=?)
     (make-heap (vector-copy vec) count <=?)]))

(define (heap-add! h . keys)
  (heap-add-all! h (list->vector keys)))

(define (heap-add-all! h keys)
  (let ([keys (if (list? keys) (list->vector keys) keys)])
    (match h
      [(heap vec size <=?)
       (let* ([new-size (+ size (vector-length keys))]
              [vec (if (> new-size (vector-length vec))
                       (let ([vec (grow-vector vec new-size)])
                         (set-heap-vec! h vec)
                         vec)
                       vec)])
         (vector-copy! vec size keys 0)
         (for ([n (in-range size new-size)])
           (heapify-up <=? vec n))
         (set-heap-size! h new-size))])))

(define (heap-min h)
  (match h
    [(heap vec size <=?)
     (when (zero? size)
       (error 'heap-min "empty heap"))
     (vector-ref vec 0)]))

(define (heap-remove-min! h)
  (match h
    [(heap vec size <+?)
     (when (zero? size)
       (error 'heap-remove-min! "empty heap"))
     (heap-remove-index! h 0)]))

(define (heap-remove-index! h index)
  (match h
    [(heap vec size <=?)
     (unless (< index size)
       (if (zero? size)
           (error 'heap-remove-index!
                  "index out of bounds (empty heap): ~s" index)
           (error 'heap-remove-index!
                  "index out of bounds [0,~s]: ~s" (sub1 size) index)))
     (vector-set! vec index (vector-ref vec (sub1 size)))
     (vector-set! vec (sub1 size) #f)
     (heapify-down <=? vec index (sub1 size))
     (when (< MIN-SIZE size (quotient (vector-length vec) 4))
       (set-heap-vec! h (shrink-vector vec)))
     (set-heap-size! h (sub1 size))]))

(define (in-heap h)
  (in-heap/consume! (heap-copy h)))

(define (in-heap/consume! h)
  (lambda ()
    (values (lambda () (heap-min h))
            (lambda () (heap-remove-min! h) #t)
            #t
            (lambda (_) (> (heap-count h) 0))
            (lambda _ #t)
            (lambda _ #t))))

(provide/contract
 [make-heap (-> (-> any/c any/c any/c) heap?)]
 [heap? (-> any/c boolean?)]
 [heap-size (-> heap? exact-nonnegative-integer?)]
 [heap-copy (-> heap? heap?)]
 [vector->heap (-> (-> any/c any/c any/c) vector? heap?)]
 [heap-add! (->* (heap?) () #:rest list? void?)]
 [heap-add-all! (-> heap? (or/c list? vector?) void?)]
 [heap-min (-> heap? any/c)]
 [heap-remove-min! (-> heap? void?)]
 [in-heap (-> heap? sequence?)])

#|
;; Testing

(vector->heap #(3 65 3 54 3 2 1 4 6))

(define h
  (vector->heap #(3 65 3 3 2 1)))
|#
