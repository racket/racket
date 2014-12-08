#lang racket

(for ([i (in-range 10000)])
  (define-values (i o) (place-channel))
  (sync/timeout 0 i)
  (sync/timeout 0 o))
