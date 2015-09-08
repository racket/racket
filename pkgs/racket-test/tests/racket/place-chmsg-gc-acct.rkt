#lang racket/base
(require racket/place)

(define (go)
  (place
   pch

   ;; Trigger memory accounting in every place:
   (custodian-limit-memory
    (current-custodian)
    (* 1024 1024 1024))

   (dynamic-require 'tests/racket/place-chmsg-gc #f)

   0))

(module+ main
  (define r
    (map place-wait
         (for/list ([i 8])
           (go))))
  (unless (andmap zero? r)
    (error "some place failed")))

(module+ test
  (require (submod ".." main)))
