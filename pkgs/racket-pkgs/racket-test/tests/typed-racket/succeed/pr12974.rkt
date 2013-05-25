#lang typed/racket

(define (f)
  (let loop ((i 5))
    (if (zero? i)
        (void)
        (loop (sub1 i))))
  2)

(let loop ((i 5))
  (if (zero? i)
      (void)
      (loop (sub1 i))))


(define (g)
  (values 1 2)
  2)
