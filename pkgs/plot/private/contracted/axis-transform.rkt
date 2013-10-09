#lang racket/base

(require racket/contract unstable/latent-contract)

(require "../common/axis-transform.rkt")
(provide (contract-out (struct invertible-function ([f (real? . -> . real?)]
                                                    [g (real? . -> . real?)])))
         (activate-contract-out id-function
                                invertible-compose
                                invertible-inverse
                                axis-transform/c
                                id-transform
                                apply-axis-transform
                                make-axis-transform
                                axis-transform-compose
                                axis-transform-append
                                axis-transform-bound
                                log-transform
                                cbrt-transform
                                hand-drawn-transform
                                stretch-transform
                                collapse-transform))
