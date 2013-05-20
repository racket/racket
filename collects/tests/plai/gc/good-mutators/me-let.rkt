#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 20)

2 4 6 8 10 12 14

(define g
  (let [[i 1] ;; 2
        [j 2] ;; 4
        [k 3] ;; 6
        [m 4] ;; 8 <-- GC
        [n 5] ;; 10
        [o 6] ;; 12
        [p 7]] ;; 14
    ;; 16
    (lambda (x) (+ i j k m n o p))))

;; 18 for the 8
(g 8)
;; 20 for the answer
