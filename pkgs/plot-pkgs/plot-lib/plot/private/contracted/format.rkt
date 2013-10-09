#lang racket/base

(require unstable/latent-contract)

(require "../common/format.rkt")
(provide (activate-contract-out
          integer->superscript
          digits-for-range
          real->decimal-string* real->string/trunc
          real->plot-label ivl->plot-label ->plot-label
          parse-format-string apply-formatter))
