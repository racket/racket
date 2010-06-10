(: fib (Integer -> Integer))
(define (fib n)
  (cond ((< n 2) 1)
        (else (+ (fib (- n 2)) (fib (- n 1))))))

(: main ((Vectorof String) -> Void))
(define (main args)
  (let ((n (if (= (vector-length args) 0)
               1
               (assert (string->number (vector-ref args 0)) exact-integer?))))
    (display (fib n))
    (newline)))

(main (current-command-line-arguments))
