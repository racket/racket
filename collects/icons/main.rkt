#lang racket/base

(require unstable/latent-contract
         "private/svg.rkt"
         "private/utils.rkt")

(provide (activate-contract-out
          icon-colors
          icon-color/c
          icon-styles
          icon-style/c
          toolbar-icon-height
          default-icon-style
          load-icon-pict
          load-icon
          format-icon-name
          icon-categories
          icon-names
          
          go-icon go-icon-pict
          bar-icon bar-icon-pict
          back-icon back-icon-pict
          stop-icon stop-icon-pict
          step-icon step-icon-pict
          step-back-icon step-back-icon-pict
          continue-icon continue-icon-pict
          continue-back-icon continue-back-icon-pict
          fast-forward-icon fast-forward-icon-pict
          rewind-icon rewind-icon-pict
          pause-icon pause-icon-pict
          stop-sign-icon stop-sign-icon-pict
          check-icon check-icon-pict
          magnifying-glass-icon magnifying-glass-icon-pict
          magnifying-glass-left-icon magnifying-glass-left-icon-pict
          disk-icon disk-icon-pict
          stop-signs-icon stop-signs-icon-pict
          macro-stepper-icon macro-stepper-icon-pict
          check-syntax-icon check-syntax-icon-pict
          check-syntax-small-icon check-syntax-small-icon-pict
          plt-logo plt-logo-pict
          planet-logo planet-logo-pict))
