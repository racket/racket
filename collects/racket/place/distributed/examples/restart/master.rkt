#lang racket/base
(require racket/place/distributed
         racket/class
         racket/place)

(provide wait-place-thunk)
(provide main)

(define (wait-place-thunk)
  (place ch
    (printf "BEGINING SLEEP\n")
    (sleep 5)
    (printf "SLEEP DONE\n")))

(define (main)
  (master-event-loop
    (spawn-vm-supervise-place-thunk-at "localhost" #:listen-port 6345 (get-current-module-path) 'wait-place-thunk #:restart-on-exit #t)
    (after-seconds 50
      (exit 0))))
