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
