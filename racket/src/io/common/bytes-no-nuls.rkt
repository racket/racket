#lang racket/base

(provide bytes-no-nuls?)

(define (bytes-no-nuls? s)
  (and (bytes? s)
       (not (for/or ([c (in-bytes s)])
              (= c 0)))))
