#lang racket/base

(require unstable/latent-contract)

(require "../common/samplers.rkt")
(provide (activate-contract-out function->sampler
                                inverse->sampler
                                2d-function->sampler
                                3d-function->sampler))
