#lang racket/base

(define (main argv)
  (let* ([n (string->number (vector-ref argv 0))]
         [hash1 (make-hash)]
         [hash2 (make-hash)]
         [zero (lambda () 0)])
    (let loop ([i 0])
      (unless (= i 10000)
        (hash-set! hash1 (string-append "foo_" (number->string i)) i)
        (loop (add1 i))))
    (let loop ([i 0])
      (unless (= i n)
        (hash-for-each hash1 (lambda (key value)
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
