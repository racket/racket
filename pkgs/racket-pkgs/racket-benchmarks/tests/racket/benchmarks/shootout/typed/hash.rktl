(: main ((Vectorof String) -> Void))
(define (main argv)
  (let*: ([n : Integer (assert (string->number (vector-ref argv 0)) exact-integer?)]
          [hash : (HashTable String Integer) (make-hash)]
          [accum : Integer 0]
          [false : ( -> False) (lambda () #f)])
    (let loop ([i 1])
      (unless (> i n)
        (hash-set! hash (number->string i 16) i)
        (loop (add1 i))))
    (let loop ([i n])
      (unless (zero? i)
        (when (hash-ref hash (number->string i) false)
          (set! accum (+ accum 1)))
        (loop (sub1 i))))
    (printf "~s\n" accum)))

(main (current-command-line-arguments))
