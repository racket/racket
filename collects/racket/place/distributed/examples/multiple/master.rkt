#lang racket/base
(require racket/place/distributed
         racket/class
         racket/place
         racket/runtime-path
         "bank.rkt"
         syntax/location)


(define-runtime-path bank-path "bank.rkt")
(define-runtime-path place-worker-path "place-worker.rkt")
(define-runtime-path process-worker-path "process-worker.rkt")

(provide main
         wait-place-thunk)

(define (spawn-place-worker-at port message)
  (spawn-node-with-place-at "localhost" #:listen-port port place-worker-path 'place-worker #:initial-message message #:restart-on-exit #f))

(define (wait-place-thunk)
  (place ch
    (printf "BEGINING SLEEP\n")
    (sleep 5)
    (printf "SLEEP DONE\n")))


(define (main)
  (define bank-node (spawn-node-with-place-at "localhost" #:listen-port 6344 bank-path 'make-bank))
  (define bank-place (send bank-node get-first-place))
  (message-router
    (spawn-place-worker-at 6341 "ONE")
    (spawn-place-worker-at 6342 "TWO")
    (spawn-place-worker-at 6343 "THREE")
    bank-node
    (spawn-node-with-place-at "localhost" #:listen-port 6345 #:thunk #t (quote-module-name) 'wait-place-thunk #:restart-on-exit #t)
    (every-seconds 3.3 (printf "Hello from every-seconds\n") (flush-output))
    (after-seconds 2
      (displayln (bank-new-account bank-place 'user0))
      (displayln (bank-add bank-place 'user0 10))
      (displayln (bank-removeM bank-place 'user0 5)))
    ))
