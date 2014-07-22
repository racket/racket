#lang racket
(require rackunit
         data/heap)

(define (mkheap) (vector->heap <= (vector 6 2 4 10 8)))

(test-equal? "heap->vector"
  (heap->vector (mkheap))
  '#(2 4 6 8 10))

(test-equal? "heap-add! min"
  (let ([h (mkheap)])
    (heap-add! h 0)
    (heap->vector h))
  '#(0 2 4 6 8 10))

(test-equal? "heap-add! mid"
  (let ([h (mkheap)])
    (heap-add! h 5)
    (heap->vector h))
  '#(2 4 5 6 8 10))

(test-equal? "heap-add! multi"
  (let ([h (mkheap)])
    (heap-add! h 0 5 12)
    (heap->vector h))
  '#(0 2 4 5 6 8 10 12))

(test-equal? "heap-remove-min!"
  (let ([h (mkheap)])
    (heap-remove-min! h)
    (heap->vector h))
  '#(4 6 8 10))

(test-equal? "heap-remove!"
  (let ([h (mkheap)])
    (heap-remove! h 4)
    (heap->vector h))
  '#(2 6 8 10))

(define (rand-test range count1 count2 count3)
  (let ([h (make-heap <=)]
        [xs null]) ;; mutated
    (define (fill! count)
      (for ([i (in-range count)])
        (let ([x (random range)])
          (heap-add! h x)
          (set! xs (cons x xs))))
      (set! xs (sort xs <)))

    (fill! count1)

    ;; check equal (non-destructive)
    (check-equal? (vector->list (heap->vector h))
                  xs)

    (for ([i (in-range count2)])
      (let ([xl (car xs)]
            [xh (heap-min h)])
        (set! xs (cdr xs))
        (heap-remove-min! h)))

    (fill! count3)

    (for ([x (in-list xs)])
      (check-equal? (heap-min h) x)
      (heap-remove-min! h))

    (check-equal? (heap-count h) 0)))
  
(test-case "heap random sparse"
  (rand-test 1000 100 50 100))

(test-case "heap random dense"
  (rand-test 20 100 50 100))

(test-equal? "in-heap"
  (for/list ([x (in-heap (mkheap))]) x)
  '(2 4 6 8 10))
(test-equal? "post in-heap count"
   (let* ([h (mkheap)]
          [lst (for/list ([x (in-heap h)]) x)])
     (heap-count h))
   (heap-count (mkheap)))
(test-equal? "in-heap/consume!"
  (for/list ([x (in-heap/consume! (mkheap))]) x)
  '(2 4 6 8 10))
(test-equal? "post in-heap/consume! count"
   (let* ([h (mkheap)]
          [lst (for/list ([x (in-heap/consume! h)]) x)])
     (heap-count h))
   0)

(test-equal? "heap-sort"
  (let ([v (vector 3 4 2 5 1)])
    (heap-sort! v <=)
    v)
  '#(1 2 3 4 5))

(test-equal? "heap-sort (old arg order)"
  (let ([v (vector 3 4 2 5 1)])
    (heap-sort! <= v)
    v)
  '#(1 2 3 4 5))

(let* ([l (for/list ([i 1000]) (random 1000))]
       [v (list->vector l)])
  (test-equal? "heap-sort (random)"
    (begin (heap-sort! v <=) (vector->list v))
    (sort l <)))

 ;; tests the heap invariant
(define (heap-good? the-heap)
  (define h (heap-copy the-heap))
  (let loop ([last -inf.0])
    (if (= 0 (heap-count h))
        #t
        (let ([item (heap-min h)])
          (heap-remove-min! h)
          (unless (and last item)
            (error 'heap-good? "Expected number in heap but found #f: ~a"
                   (with-output-to-string (λ () (pretty-print (heap->vector h))))))
          (and (<= last item)
               (loop item))))))

(define (heap-invariant h <=)
  (define vec (heap->vector h))
  (define size (heap-count h))
  (define (cmp v idx)
    (or (>= idx size)
        (let ([v* (vector-ref vec idx)])
          (unless (and v v*)
            (error 'heap-invariant "Expected number in heap, but found #f: ~a"
                   (with-output-to-string (λ () (pretty-print (heap->vector h))))))
          (<= v v*))))
  (let check ([idx 0])
    (or (>= idx size)
        (let ([idx-key (vector-ref vec idx)]
              [left (+ (* idx 2) 1)]
              [right (+ (* idx 2) 2)])
          (and (cmp idx-key left)
               (cmp idx-key right)
               (check left)
               (check right))))))
 
;; PR 14651
;; sequence of heap operations leading to failure extracted from app
;; commenting just about any of the ops will eliminate the failure
(test-not-exn
 "add/remove stress"
 (λ ()
    (define (do-ops n [dist #hash((add . 70/100)
                                  (remove-min . 20/100)
                                  (remove-random . 10/100))])
      (define h (make-heap <=))
      (define padd (hash-ref dist 'add))
      (define premove-min (+ padd (hash-ref dist 'remove-min)))
      (define premove-random (+ premove-min (hash-ref dist 'remove-random)))
      (define heap-contents (make-hash))
      (define (hash-add! v)
        (if (hash-has-key? heap-contents v)
            (hash-set! heap-contents v (add1 (hash-ref heap-contents v)))
            (hash-set! heap-contents v 1)))
      (define (hash-remove-count! v)
        (match (hash-ref heap-contents v)
          [1 (hash-remove! heap-contents v)]
          [n (hash-set! heap-contents v (sub1 n))]))
      (define action (box #f))
      (define actions (box '()))
      (for ([i (in-range n)])
        (define poke (random))
        (with-handlers ([exn:fail:contract?
                         (λ (e)
                            (error 'do-ops "Internal heap error triggered. Heap status ~a~%Heap ops~%~a"
                                   (with-output-to-string (λ () (pretty-print (heap->vector h))))
                                   (unbox actions)))])
          (cond [(< poke padd)
                 (define v (- (random 20000) 10000))
                 (set-box! action `(add ,v))
                 (hash-add! v)
                 (heap-add! h v)]
                [else               
                 (cond
                  [(= 0 (heap-count h)) (set-box! actions (cons 'spurious-removal (unbox actions)))]
                  [(< poke premove-min)
                   (define v (heap-min h))
                   (set-box! action `(remove-min = ,v))
                   (hash-remove-count! v)
                   (heap-remove-min! h)]
                  [else
                   (define which-idx (random (hash-count heap-contents)))
                   (define v
                     (for/first ([k (in-hash-keys heap-contents)]
                                 [i (in-naturals)]
                                 #:when (= i which-idx))
                       k))
                   (set-box! action `(remove ,v))
                   (hash-remove-count! v)
                   (heap-remove! h v)])])
          (set-box! actions (cons (unbox action) (unbox actions))))
        (unless (for/and ([i (in-vector (heap->vector h))]) (integer? i))
          (error 'add/remove-stress "#f introduced after ~a" (reverse (unbox actions))))
        (unless (heap-invariant h <=)
          (error 'add/remove-stress "Invariant broken after ~a" (reverse (unbox actions))))
        (unless (heap-good? h)
          (error 'add/remove-stress "Heap goodness broken after ~a" (reverse (unbox actions))))))
    (do-ops 20)
    (do-ops 50)
    (do-ops 200)))
(test-not-exn
 "add/remove from Javes Gaver"
 (λ ()
    (define h (make-heap <=))
    (define-syntax-rule (do-all e ...)
      (begin (begin e (unless (and (heap-invariant h <=)
                                   (heap-good? h))
                        (error 'add/remove-test "Bad after ~a~%" 'e))) ...))
    (do-all  
     (heap-add! h 0)
     (heap-add! h -5942)
     (heap-add! h 8358)
     (heap-add! h 569)
     (heap-add! h 6723)
     (heap-add! h -151)
     (heap-add! h 6807)
     (heap-add! h -1612)
     (heap-remove-min! h)
     (heap-add! h -1008)
     (heap-add! h -7157)
     (heap-add! h -1734)
     (heap-add! h 6497)
     (heap-add! h 1603)
     (heap-add! h -7927)
     (heap-remove! h -151)
     (heap-add! h -349)
     (heap-add! h -7570)
     (heap-remove-min! h)
     (heap-add! h 4008)
     (heap-add! h 6101)
     (heap-add! h -9013)
     (heap-add! h -3447)
     (heap-add! h -4294)
     (heap-add! h 8187)
     (heap-add! h 1465)
     (heap-remove-min! h)
     (heap-add! h -1598)
     (heap-add! h 9730)
     (heap-add! h -4429)
     (heap-add! h -846)
     (heap-add! h 4775)
     (heap-add! h 3609)
     (heap-add! h -3881)
     (heap-add! h 6167)
     (heap-add! h 6767)
     (heap-remove-min! h)
     (heap-add! h 2842)
     (heap-add! h -4103)
     (heap-add! h 154)
     (heap-add! h 3748)
     (heap-add! h -536)
     (heap-add! h -5565)
     (heap-add! h 4970)
     (heap-add! h 4775)
     (heap-add! h 4818)
     (heap-add! h 5124)
     (heap-add! h -8657)
     (heap-add! h -6842)
     (heap-remove-min! h)
     (heap-add! h 2480)
     (heap-add! h 8878)
     (heap-add! h -1806)
     (heap-remove-min! h)
     (heap-add! h -8205)
     (heap-remove! h 9730)
     (heap-add! h -3164)
     (heap-add! h 1589)
     (heap-add! h 8444)
     (heap-add! h -7839)
     (heap-add! h -3810)
     (heap-remove! h 4970))))
