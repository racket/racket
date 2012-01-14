#lang racket/base

(require racket/class racket/draw racket/promise
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         (rename-in "private/logos.rkt"
                    [plt-logo uncached-plt-logo]
                    [planet-logo uncached-planet-logo])
         "private/flomap.rkt"
         "compile-time.rkt"
         (for-syntax racket/base
                     (rename-in "private/logos.rkt"
                                [plt-logo uncached-plt-logo]
                                [planet-logo uncached-planet-logo])
                     "private/flomap.rkt"))

(provide (activate-contract-out
          plt-logo plt-flomap
          planet-logo planet-flomap)
         (only-doc-out (all-from-out "private/logos.rkt"))
         (only-doc-out (all-defined-out)))

;; Use a delay to keep from using more memory than necessary (saves 256KB)
(define standard-plt-logo (delay (compiled-bitmap (uncached-plt-logo 256))))

(defproc (plt-logo [height (and/c rational? (>=/c 0)) 256]) (is-a?/c bitmap%)
  (cond [(height . = . 256)  (force standard-plt-logo)]
        [(height . <= . 256)
         (flomap->bitmap (flomap-resize (bitmap->flomap (force standard-plt-logo)) #f height))]
        [else
         (uncached-plt-logo height)]))


(define standard-planet-logo (delay (compiled-bitmap (uncached-planet-logo 256))))

(defproc (planet-logo [height (and/c rational? (>=/c 0)) 256]) (is-a?/c bitmap%)
  (cond [(height . = . 256)  (force standard-planet-logo)]
        [(height . <= . 256)
         (flomap->bitmap (flomap-resize (bitmap->flomap (force standard-planet-logo)) #f height))]
        [else
         (uncached-planet-logo height)]))
