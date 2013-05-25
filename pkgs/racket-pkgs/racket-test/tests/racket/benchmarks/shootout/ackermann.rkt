#lang racket/base
(require racket/cmdline)

(define (ack m n)
  (cond ((zero? m) (+ n 1))
        ((zero? n) (ack (- m 1) 1))
        (else      (ack (- m 1) (ack m (- n 1))))))

(command-line #:args (n)
              (printf "Ack(3,~a): ~a\n" 
                      n
                      (ack 3 (string->number n))))
