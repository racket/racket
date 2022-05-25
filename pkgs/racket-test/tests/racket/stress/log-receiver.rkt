#lang racket/base

(module+ test
  (module config info
    (define timeout 1000)))

(define (do-thread t) (thread t))
(set! do-thread do-thread)

(let loop ([i 0])
  (unless (= i 2000000)
    (when (zero? (modulo i 10000))
      (printf "~s\n" i))
    (define (spin-a-while)
      (let loop ([j (random (add1 (modulo i 100000)))])
        (unless (zero? j)
          (loop (sub1 j)))))
    (define s (make-log-receiver (current-logger) 'info 'send))
    (define t
      (do-thread
       (lambda ()
         (log-message (current-logger) 'info 'send "a" 1))))
    (spin-a-while)
    (unless (vector? (sync s))
      (error "not a vector result!"))
    (thread-wait t)
    (loop (add1 i))))
