#lang racket

(define print-17
  (lambda () (printf "This is 17.\n")))

(module+ sub
  (with-output-to-file "stdout"
    print-17
    #:exists 'append))
