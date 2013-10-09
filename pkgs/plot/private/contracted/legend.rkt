#lang racket/base

(require racket/contract unstable/latent-contract racket/class)

(require "../common/legend.rkt"
         "../common/plot-device.rkt")
(provide (contract-out
          (struct legend-entry ([label string?]
                                [draw ((is-a?/c plot-device%) real? real? . -> . void?)])))
         (activate-contract-out
          line-legend-entry line-legend-entries
          rectangle-legend-entry rectangle-legend-entries
          interval-legend-entry interval-legend-entries
          point-legend-entry
          arrow-legend-entry))
