#lang racket/base
(module+ test
  (module config info
    (define timeout 300)))

(define (go n)
  (for ([i n])
    (define c (make-custodian))
    (custodian-limit-memory c (* 1024 1024 100) c)
    (parameterize ([current-custodian c])
      (thread (lambda () (sync (make-semaphore)))))
    (sync (system-idle-evt))
    (custodian-shutdown-all c)))

(define (get-use)
  (sync (system-idle-evt))
  (collect-garbage)
  (current-memory-use))

(go 1000)
(define start-use (current-memory-use))

(go 100000)
(define end-use (current-memory-use))

(unless (end-use . < . (* 2 start-use))
  (error "something accumulated too much: " start-use end-use))
