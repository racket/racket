#lang racket/base

(define (main args)
  (let ((n (if (= (vector-length args) 0)
               1
               (string->number (vector-ref args 0))))
        (count 0)
        (flags (make-vector 8192)))
    (let loop ((iter n))
      (if (> iter 0)
          (begin
            (do ((i 0 (+ i 1))) ((>= i 8192)) (vector-set! flags i #t))
            (set! count 0)
            (do ((i 2 (+ 1 i)))
                ((>= i 8192))
              (if (vector-ref flags i)
                  (begin
                    (do ((k (+ i i) (+ k i)))
                        ((>= k 8192))
                      (vector-set! flags k #f))
                    (set! count (+ 1 count)))
                  #t))
            (loop (- iter 1)))
          #t))
    (display "Count: ") (display count) (newline)))

(main (current-command-line-arguments))
