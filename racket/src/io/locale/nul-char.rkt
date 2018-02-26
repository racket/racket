#lang racket/base

(provide string-length-up-to-nul
         maybe-substring)

;; Get the number of characters available before a nul character
(define (string-length-up-to-nul s i l)
  (let loop ([j i])
    (cond
      [(= j l) (- j i)]
      [(eqv? (string-ref s j) #\nul) (- j i)]
      [else (loop (add1 j))])))


(define (maybe-substring s i l)
  (if (zero? i)
      s
      (substring s i l)))
