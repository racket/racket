#lang racket/base

(define (fib n)
  (cond ((< n 2) 1)
        (else (+ (fib (- n 2)) (fib (- n 1))))))

(define (main args)
  (let ((n (if (= (vector-length args) 0)
               1
               (string->number (vector-ref args 0)))))
    (display (fib n))
    (newline)))

(main (current-command-line-arguments))
