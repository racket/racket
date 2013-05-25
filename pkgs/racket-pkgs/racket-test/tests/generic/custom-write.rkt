#lang racket/base

(require racket/port)

(struct loud (v)
        #:methods gen:custom-write
        [(define (write-proc x port mode)
           (displayln "writing!" port)
           (case mode
             [(#t) (write (loud-v x) port)]
             [(#f) (display (loud-v x) port)]
             [else (print (loud-v x) port mode)]))])

(module+ test
 (require rackunit)

 (check-equal? (with-output-to-string (lambda () (write (loud 1))))
               "writing!\n1")
 (check-equal? (with-output-to-string (lambda () (display (loud 1))))
               "writing!\n1")
 (check-equal? (with-output-to-string (lambda () (print (loud 1))))
               "writing!\n1"))
