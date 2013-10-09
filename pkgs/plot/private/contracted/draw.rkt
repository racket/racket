#lang racket/base

(require unstable/latent-contract)

(require "../common/draw.rkt")
(provide (activate-contract-out ->color ->pen-color ->brush-color ->pen-style ->brush-style
                                color-seq color-seq*
                                alpha-expt))
