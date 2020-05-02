;; Iterations for slow things:
(define Q 1000000)

;; The depth used for non-tail recursion, typically:
(define M (* Q 10))

;; Intermediate count:
(define L (* M 10))

;; Number of iteraitons used for a loop, typically
(define N (* L 10))

;; Number of times to run each benchmark:
(define I 3)

(define-syntax times
  (syntax-rules ()
    [(_ e)
     (let loop ([v #f] [i I])
       (if (zero? i)
           v
           (loop (time e) (sub1 i))))]))
