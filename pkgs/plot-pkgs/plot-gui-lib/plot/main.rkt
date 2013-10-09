#lang racket/base

(require unstable/latent-contract
         plot/no-gui
         "private/gui/plot2d.rkt"
         "private/gui/plot3d.rkt")

(provide (all-from-out plot/no-gui)
         (activate-contract-out plot-snip plot-frame plot)
         (activate-contract-out plot3d-snip plot3d-frame plot3d))
