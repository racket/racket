
;; This variant of the benchmark uses `/'.
;; See "collatz-q.sch" for the `quotient' variant.

(: cycle-length : Integer -> Integer)
(define (cycle-length n)
  (cond
   [(= n 1)
    1]
   [(odd? n)
    (+ 1 (cycle-length (+ 1 (* 3 n))))]
   [(even? n)
    ;; TR note: quotient would work better, but that's the other version of
    ;;          the benchmark
    (+ 1 (cycle-length (assert (/ n 2) integer?)))]
   [else (error "unreachable")]))

(time (let: loop : (U False Integer)
        ([i : Integer 1] [v : (U False Integer) #f])
        (if (= i 1000000)
            v
            (loop (+ i 1) (cycle-length i)))))

