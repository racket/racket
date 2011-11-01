#lang racket/base

(require racket/contract unstable/latent-contract racket/class)

(require "../common/legend.rkt"
         "../common/area.rkt")
(provide (contract-out
          (struct legend-entry ([label string?]
                                [draw ((is-a?/c plot-area%) real? real? real? real? . -> . void?)])))
         (activate-contract-out
          line-legend-entry line-legend-entries
          rectangle-legend-entry rectangle-legend-entries
          interval-legend-entry interval-legend-entries
          contour-intervals-legend-entries
          point-legend-entry
          vector-field-legend-entry))
