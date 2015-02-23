#lang racket/base

(define (main argv)
  (let* ([n (string->number (vector-ref argv 0))]
         [hash (make-hash)]
         [accum 0]
         [false (lambda () #f)])
    (let loop ([i 1])
      (unless (> i n)
        (hash-set! hash (number->string i 16) i)
        (loop (add1 i))))
    (let loop ([i n])
      (unless (zero? i)
        (when (hash-ref hash (number->string i) false)
          (set! accum (+ accum 1)))
        (loop (sub1 i))))
    (printf "~s\n" accum)))

(main (current-command-line-arguments))
