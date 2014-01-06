#lang racket/base

(provide chomp)

;; removes newline (if any) at end of string
(define (chomp str)
  (define len (string-length str))
  (if (eq? #\newline (string-ref str (sub1 len)))
      (substring str 0 (sub1 len))
      str))

