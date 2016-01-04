#lang racket/base
(require racket/place)

(define (go)
  (place pch
         (place-channel-put pch 28)))

(module+ main                                                                                       
  (define p (go))
  (define n (place-channel-get p))
  (void (place-wait p))
  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    (lambda () (printf "~a\n" n))
    #:exists 'append))
