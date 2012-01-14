#lang racket/base

(require racket/promise
         (prefix-in private- "private/logos.rkt")
         "private/flomap.rkt"
         "compile-time.rkt"
         (for-syntax racket/base
                     (prefix-in private- "private/logos.rkt")
                     "private/flomap.rkt"))

(provide plt-logo planet-logo
         (rename-out [private-plt-flomap plt-flomap]
                     [private-planet-flomap planet-flomap]))

;; Use a delay to keep from using more memory than necessary (saves 256KB)
(define standard-plt-logo (delay (compiled-bitmap (private-plt-logo 256))))

(define (plt-logo height)
  (cond [(height . = . 256)  (force standard-plt-logo)]
        [(height . <= . 256)
         (flomap->bitmap (flomap-resize (bitmap->flomap (force standard-plt-logo)) #f height))]
        [else
         (private-plt-logo height)]))


(define standard-planet-logo (delay (compiled-bitmap (private-planet-logo 256))))

(define (planet-logo height)
  (cond [(height . = . 256)  (force standard-planet-logo)]
        [(height . <= . 256)
         (flomap->bitmap (flomap-resize (bitmap->flomap (force standard-planet-logo)) #f height))]
        [else
         (private-planet-logo height)]))
