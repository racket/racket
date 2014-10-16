#lang racket
(require rackunit
         data/heap
         (submod data/heap test-util))

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

(define (heap-add!/v h v)
  (heap-add! h v)
  (unless (valid-heap? h) (error 'heap-add!/v "failed")))
(define (heap-remove!/v h v)
  (heap-remove! h v)
  (unless (valid-heap? h) (error 'heap-remove!/v "post failed")))
(define (heap-remove-min!/v h)
  (heap-remove-min! h)
  (unless (valid-heap? h) (error 'heap-remove-min!/v "post failed")))

;; test case from PR14651
(let ([h (make-heap <=)])
  (heap-add!/v h 0)
  (heap-add!/v h -5942)
  (heap-add!/v h 8358)
  (heap-add!/v h 569)
  (heap-add!/v h 6723)
  (heap-add!/v h -151)
  (heap-add!/v h 6807)
  (heap-add!/v h -1612)
  (heap-remove-min!/v h)
  (heap-add!/v h -1008)
  (heap-add!/v h -7157)
  (heap-add!/v h -1734)
  (heap-add!/v h 6497)
  (heap-add!/v h 1603)
  (heap-add!/v h -7927)
  (heap-remove!/v h -151)
  (heap-add!/v h -349)
  (heap-add!/v h -7570)
  (heap-remove-min!/v h)
  (heap-add!/v h 4008)
  (heap-add!/v h 6101)
  (heap-add!/v h -9013)
  (heap-add!/v h -3447)
  (heap-add!/v h -4294)
  (heap-add!/v h 8187)
  (heap-add!/v h 1465)
  (heap-remove-min!/v h)
  (heap-add!/v h -1598)
  (heap-add!/v h 9730)
  (heap-add!/v h -4429)
  (heap-add!/v h -846)
  (heap-add!/v h 4775)
  (heap-add!/v h 3609)
  (heap-add!/v h -3881)
  (heap-add!/v h 6167)
  (heap-add!/v h 6767)
  (heap-remove-min!/v h)
  (heap-add!/v h 2842)
  (heap-add!/v h -4103)
  (heap-add!/v h 154)
  (heap-add!/v h 3748)
  (heap-add!/v h -536)
  (heap-add!/v h -5565)
  (heap-add!/v h 4970)
  (heap-add!/v h 4775)
  (heap-add!/v h 4818)
  (heap-add!/v h 5124)
  (heap-add!/v h -8657)
  (heap-add!/v h -6842)
  (heap-remove-min!/v h)
  (heap-add!/v h 2480)
  (heap-add!/v h 8878)
  (heap-add!/v h -1806)
  (heap-remove-min!/v h)
  (heap-add!/v h -8205)
  (heap-remove!/v h 9730)
  (heap-add!/v h -3164)
  (heap-add!/v h 1589)
  (heap-add!/v h 8444)
  (heap-add!/v h -7839)
  (heap-add!/v h -3810)
  (heap-remove!/v h 4970)
  ; -1612 out of position
  (void))

;; simpler test case from PR14651
(let ([heap (make-heap <=)])
  (heap-add!/v heap 43)
  (heap-add!/v heap 1)
  (heap-add!/v heap 37)
  (heap-add!/v heap 81)
  (heap-add!/v heap 94)
  (heap-add!/v heap 4)
  (heap-remove!/v heap 94))

(define (random-test)
  (define heap (make-heap <=))
  (let loop ([ops '()]
             [values '()])
    (cond
      [(not (valid-heap? heap))
       (eprintf "crash! ~a ops\n" (length ops))
       (pretty-write `(let ([heap (make-heap <=)]) ,@(reverse ops))
                     (current-error-port))]
      [(= (length ops) 50)
       (void)]
      [else
       (define (do-an-add)
         (define n (random 10))
         (heap-add! heap n)
         (loop (cons `(heap-add!/v heap ,n) ops)
               (cons n values)))
       (case (random 3)
         [(0) (do-an-add)]
         [(1)
          (cond
            [(null? values)
             (do-an-add)]
            [else
             (define to-remove (list-ref values (random (length values))))
             (heap-remove! heap to-remove)
             (loop (cons `(heap-remove!/v heap ,to-remove) ops)
                   (remove to-remove values))])]
         [(2)
          (cond
            [(null? values)
             (do-an-add)]
            [else
             (heap-remove-min! heap)
             (define smallest (apply min values))
             (loop (cons `(heap-remove-min!/v heap) ops)
                   (remove smallest values))])])])))

(for ([x (in-range 10000)])
  (random-test))
