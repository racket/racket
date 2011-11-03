#lang racket/base

(require racket/contract unstable/latent-contract)

(require "../common/sample.rkt")
(provide (contract-out (struct sample ([xs (listof real?)]
                                       [ys (listof real?)]
                                       [y-min (or/c real? #f)]
                                       [y-max (or/c real? #f)]))
                       (struct 2d-sample ([xs (listof real?)]
                                          [ys (listof real?)]
                                          [zss (vectorof (vectorof real?))]
                                          [z-min (or/c real? #f)]
                                          [z-max (or/c real? #f)]))
                       (struct 3d-sample ([xs (listof real?)]
                                          [ys (listof real?)]
                                          [zs (listof real?)]
                                          [dsss (vectorof (vectorof (vectorof real?)))]
                                          [d-min (or/c real? #f)]
                                          [d-max (or/c real? #f)])))
         (activate-contract-out build-linear-seq linear-seq linear-seq* nonlinear-seq
                                sampler/c 2d-sampler/c 3d-sampler/c
                                make-function->sampler
                                make-2d-function->sampler
                                make-3d-function->sampler)
         (contract-out (struct mapped-function ([f (any/c . -> . any/c)]
                                                [fmap ((listof any/c) . -> . (listof any/c))])))
         map*)
