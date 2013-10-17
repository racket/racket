(: main ((Vectorof String) -> Void))
(define (main argv)
  (let*: ([n : Integer (assert (string->number (vector-ref argv 0)) exact-integer?)]
          [hash1 : (HashTable String Integer) (make-hash)]
          [hash2 : (HashTable String Integer) (make-hash)]
          [zero : ( -> 0) (lambda () 0)])
    (let loop ([i 0])
      (unless (= i 10000)
        (hash-set! hash1 (string-append "foo_" (number->string i)) i)
        (loop (add1 i))))
    (let loop ([i 0])
      (unless (= i n)
        (hash-for-each hash1 (lambda: ((key : String) (value : Integer))
                               (hash-set!
                                hash2
                                key
                                (+ (hash-ref hash2 key zero) value))))
        (loop (add1 i))))
    (printf "~s ~s ~s ~s\n"
            (hash-ref hash1 "foo_1")
            (hash-ref hash1 "foo_9999")
            (hash-ref hash2 "foo_1")
            (hash-ref hash2 "foo_9999"))))

(main (current-command-line-arguments))
