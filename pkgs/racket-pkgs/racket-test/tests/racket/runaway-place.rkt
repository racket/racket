#lang racket/base
(require racket/place)

(module+ test
  (main))

(define (main)
  (parameterize ([current-custodian (make-custodian)])
    (custodian-limit-memory (current-custodian) (* 1024 1024 64))
    (parameterize ([current-custodian (make-custodian)])
      (place-wait (place ch (runaway))))))

(define (runaway)
  (printf "starting\n")
  (define p (place ch (runaway)))
  (place-wait p))

