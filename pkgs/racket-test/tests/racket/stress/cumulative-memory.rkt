#lang racket/base

;; Check whether `(current-memory-use 'cumulative)` is always increasing

(define (work n)
  (length (let loop ([n n])
            (if (zero? n)
                '()
                (cons (make-vector n) (loop (sub1 n)))))))

(let loop ([n 20000] [u (current-memory-use 'cumulative)])
  (unless (zero? n)
    (work (random 1000))
    (let ([u2 (current-memory-use 'cumulative)])
      (if (u2 . < . u)
          (error "oops")
          (loop (sub1 n) u2)))))
