#lang racket/base

(define (main args)
  (let* ((n (if (= (vector-length args) 0)
                1
                (string->number (vector-ref args 0))))
         (x (make-vector n 0))
         (y (make-vector n 0))
         (last (- n 1)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (vector-set! x i (+ i 1)))
    (do ((k 0 (+ k 1)))
        ((= k 1000))
      (do ((i last (- i 1)))
          ((< i 0))
        (vector-set! y i (+ (vector-ref x i) (vector-ref y i)))))
    (print-list (vector-ref y 0) " " (vector-ref y last))))

(define (print-list . items) (for-each display items) (newline))

(main (current-command-line-arguments))
