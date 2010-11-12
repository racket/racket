#lang racket

(define n (command-line #:args (n) (string->number n)))

(for ([i (in-range n)])
  (fprintf (if (even? i)
               (current-error-port)
               (current-output-port))
           "~a\n"
           i))

