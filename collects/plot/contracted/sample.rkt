#lang racket/base

(require racket/contract unstable/latent-contract)

(require "../common/sample.rkt"
         "../common/math.rkt")
(provide (contract-out (struct sample ([xs (listof real?)]
                                       [ys (listof real?)]
                                       [y-min (or/c rational? #f)]
                                       [y-max (or/c rational? #f)]))
                       (struct 2d-sample ([xs (listof real?)]
                                          [ys (listof real?)]
                                          [zss (vectorof (vectorof real?))]
                                          [z-min (or/c rational? #f)]
                                          [z-max (or/c rational? #f)]))
                       (struct 3d-sample ([xs (listof real?)]
                                          [ys (listof real?)]
                                          [zs (listof real?)]
                                          [dsss (vectorof (vectorof (vectorof real?)))]
                                          [d-min (or/c rational? #f)]
                                          [d-max (or/c rational? #f)])))
         (activate-contract-out build-linear-seq linear-seq linear-seq* nonlinear-seq
                                sampler/c 2d-sampler/c 3d-sampler/c
                                make-function->sampler
                                make-2d-function->sampler
                                make-3d-function->sampler
                                sample-exact->inexact
                                2d-sample-exact->inexact
                                3d-sample-exact->inexact
                                flonum-ok-for-2d?
                                flonum-ok-for-3d?
                                flonum-ok-for-4d?
                                )
         (contract-out (struct mapped-function ([f (any/c . -> . any/c)]
                                                [fmap ((listof any/c) . -> . (listof any/c))])))
         map*
         for-2d-sample for-3d-sample)
