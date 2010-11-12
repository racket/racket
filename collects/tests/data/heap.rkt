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
