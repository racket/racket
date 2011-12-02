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
          
          go-icon-pict
          bar-icon-pict
          back-icon-pict
          stop-icon-pict
          step-icon-pict
          step-back-icon-pict
          continue-icon-pict
          continue-back-icon-pict
          fast-forward-icon-pict
          rewind-icon-pict
          pause-icon-pict
          stop-sign-icon-pict
          check-icon-pict
          magnifying-glass-icon-pict
          magnifying-glass-left-icon-pict
          stop-signs-icon-pict
          macro-stepper-icon-pict
          check-syntax-icon-pict
          check-syntax-small-icon-pict
          plt-logo-pict
          planet-logo-pict
          
          go-icon
          bar-icon
          back-icon
          stop-icon
          step-icon
          step-back-icon
          continue-icon
          continue-back-icon
          fast-forward-icon
          rewind-icon
          pause-icon
          stop-sign-icon
          check-icon
          magnifying-glass-icon
          magnifying-glass-left-icon
          stop-signs-icon
          macro-stepper-icon
          check-syntax-icon
          check-syntax-small-icon
          plt-logo
          planet-logo))
