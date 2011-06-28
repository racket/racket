#lang racket
(require tests/eli-tester
         "../lib/pqueue.rkt")

(define N 10)

(test
 (local [(define pq
           (pqueue (make-temporary-file "tmp~a" 'directory)))]
   (test (pqueue-init! pq)
         
         (for ([i (in-range N)])
           (pqueue-enqueue! pq i))
         
         (for/list ([i (in-range N)])
           (pqueue-dequeue! pq))
         =>
         (for/list ([i (in-range N)]) i))))
