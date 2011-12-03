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
          default-icon-height
          default-icon-style
          load-icon-pict
          load-icon
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
          x-icon x-icon-pict
          magnifying-glass-icon magnifying-glass-icon-pict
          magnifying-glass-left-icon magnifying-glass-left-icon-pict
          disk-icon disk-icon-pict
          up-arrow-icon up-arrow-icon-pict
          down-arrow-icon down-arrow-icon-pict
          left-arrow-icon left-arrow-icon-pict
          right-arrow-icon right-arrow-icon-pict
          stop-signs-icon stop-signs-icon-pict
          earth-icon earth-icon-pict
          moon-icon moon-icon-pict
          hash-quote-icon hash-quote-icon-pict
          plus-icon plus-icon-pict
          plt-logo plt-logo-pict
          planet-logo planet-logo-pict))
