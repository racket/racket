#lang racket/base
(require racket/cmdline)

(define N 25)

;; create an eq?-fresh object
(define (alloc i)
  (number->string i))

(define (check status)
  (define mem (quotient (current-memory-use) #e1e6))
  (printf "memory use: ~s\n" mem)
  (cond
   [(and status ((vector-ref status 0) . < . mem))
    (if ((vector-ref status 1) . > . 3)
        (error "memory use grew too many times")
        (vector mem (add1 (vector-ref status 1))))]
   [status status]
   [else (vector mem 0)]))
    
 (quotient (current-memory-use) #e1e6)

(define (test-mutable)
  (define h (make-hasheq))
  (for/fold ([status #f]) ([i N])
    (collect-garbage)
    (collect-garbage)
    (for ([i #e1e6])
      (define p (alloc i))
      (hash-set! h p #t)
      (hash-remove! h p))
    (check status)))

(define (test-immutable)
  (define h #hasheq())
  (for/fold ([status #f]) ([i N])
    (collect-garbage)
    (collect-garbage)
    (for ([i #e1e6])
      (define p (alloc i))
      (set! h (hash-set h p #t))
      (set! h (hash-remove h p)))
    (check status)))

(test-mutable)
(test-immutable)
