#lang racket/base
(require racket/place)

(define (churn n)
  (let loop ([l (cons "" 0)])
    (if (equal? (cdr l) n)
        (string-length (car l))
        (loop (cons (make-string 512 #\x)
                    (add1 (cdr l)))))))

(define (go)
  (place pch
         (churn 100000)))

(module+ main
  (go)
  (define receiver
    (make-log-receiver (current-logger) 'debug 'GC))
  (void
   (thread
    (lambda ()
      (let loop ()
        (sync receiver)
        (loop)))))
  (churn 100000))
