#lang racket/base

(require racket/contract unstable/latent-contract)

(require "../common/sample.rkt")
(provide (activate-contract-out build-linear-seq linear-seq linear-seq* nonlinear-seq
                                sample/c sampler/c
                                2d-sample/c 2d-sampler/c
                                3d-sample/c 3d-sampler/c
                                make-function->sampler
                                make-2d-function->sampler
                                make-3d-function->sampler
                                2d-sample->list 3d-sample->list)
         (contract-out (struct mapped-function ([f (any/c . -> . any/c)]
                                                [fmap ((listof any/c) . -> . (listof any/c))])))
         map*)
