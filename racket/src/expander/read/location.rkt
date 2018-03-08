#lang racket/base

(provide port-next-location*)

(define (port-next-location* in init-c)
  ;; If we've already read `init-c`, then back up by one column and
  ;; position; we assume that `init-c` is not a newline character
  (cond
   [(not init-c) (port-next-location in)]
   [else
    (define-values (line col pos) (port-next-location in))
    (values line
            (and col (max 0 (sub1 col)))
            (and pos (max 1 (sub1 pos))))]))
