
(: cycle-length : Integer -> Integer)
(define (cycle-length n)
  (cond
   [(= n 1)
    1]
   [(odd? n)
    (+ 1 (cycle-length (+ 1 (* 3 n))))]
   [(even? n)
    (+ 1 (cycle-length (quotient n 2)))]
   [else (error "unreachable")]))

(time (let: loop : (U False Integer)
        ([i : Integer 1] [v : (U False Integer) #f])
        (if (= i 1000000)
            v
            (loop (+ i 1) (cycle-length i)))))

