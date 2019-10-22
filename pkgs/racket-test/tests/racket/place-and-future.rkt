#lang racket

(define (go)
  (place pch (run (place-channel-get pch))))

(define (run mode)
  (define ht (make-hasheq))
  (define f (future (lambda ()
                      (let loop ()
                        (case mode
                          [(spin) (loop)]
                          [(sync) (hash-set! ht 'ok #t)
                                  (loop)]
                          [(finish) (void)])))))
  (sleep 0.1))

(define (run-test)
  (for* ([mode '(spin sync finish)]
         [i (in-range 10)])
    (printf "go ~s ~s\n" mode i)
    (for-each
     place-wait
     (for/list ([i (in-range 2)])
       (define pl (go))
       (place-channel-put pl mode)
       pl))))

(module+ main
  (run-test))

(module+ test
  (run-test))
