#lang racket/base
(require racket/place/distributed
         racket/class
         racket/place
         syntax/location)

(provide wait-place-thunk)
(provide main)

(define (wait-place-thunk)
  (place ch
    (printf "BEGINING SLEEP\n")
    (sleep 5)
    (printf "SLEEP DONE\n")))

(define (main)
  (message-router
    (spawn-vm-with-place-thunk-at "localhost" #:listen-port 6345 (quote-module-name) 'wait-place-thunk #:restart-on-exit #t)
    (after-seconds 50
      (exit 0))))
