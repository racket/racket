#lang racket/base

(require racket/contract unstable/latent-contract)

(require "../common/date-time.rkt")
(provide (contract-out (struct plot-time ([second (and/c (>=/c 0) (</c 60))]
                                          [minute (integer-in 0 59)]
                                          [hour (integer-in 0 23)]
                                          [day exact-integer?])))
         (activate-contract-out plot-time->seconds seconds->plot-time
                                datetime->real))
