
;; This variant of the benchmark uses `/'.
;; See "collatz-q.sch" for the `quotient' variant.

(define (cycle-length n)
  (cond
   [(= n 1)
    1]
   [(odd? n)
    (+ 1 (cycle-length (+ 1 (* 3 n))))]
   [(even? n)
    (+ 1 (cycle-length (/ n 2)))]))

(time (let loop ([i 1] [v #f])
        (if (= i 1000000)
            v
            (loop (+ i 1) (cycle-length i)))))

