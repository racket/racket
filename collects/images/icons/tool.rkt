#lang racket/base

(require racket/draw racket/class racket/math racket/sequence
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "control.rkt"
         "symbol.rkt"
         "misc.rkt"
         "style.rkt")

(provide debugger-bomb-color
         macro-stepper-hash-color
         small-macro-stepper-hash-color
         (activate-contract-out
          check-syntax-icon check-syntax-flomap
          small-check-syntax-icon small-check-syntax-flomap
          macro-stepper-icon macro-stepper-flomap
          small-macro-stepper-icon small-macro-stepper-flomap
          debugger-icon debugger-flomap
          small-debugger-icon small-debugger-flomap)
         (only-doc-out (all-defined-out)))

(defthing debugger-bomb-color (or/c string? (is-a?/c color%)) #:document-value
  (make-object color% 128 32 32))

;; Actual color is too dark after rendering
(defthing macro-stepper-hash-color (or/c string? (is-a?/c color%)) #:document-value
  (make-object color% 60 192 60))
(defthing small-macro-stepper-hash-color (or/c string? (is-a?/c color%)) #:document-value
  (make-object color% 128 255 128))

(defproc (check-syntax-flomap [height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
                              [material deep-flomap-material-value? (default-icon-material)]
                              ) flomap?
  (flomap-ht-append
   (left-magnifying-glass-flomap metal-icon-color "chocolate" height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/32 height)))) 0)
   (check-flomap syntax-icon-color height material)))

(defproc (small-check-syntax-flomap [height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
                                    [material deep-flomap-material-value? (default-icon-material)]
                                    ) flomap?
  (flomap-pin*
   1 1 5/16 1
   (check-flomap syntax-icon-color height material)
   (magnifying-glass-flomap metal-icon-color "chocolate" (* 3/4 height) material)))

(defproc (macro-stepper-flomap [height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
                               [material deep-flomap-material-value? (default-icon-material)]
                               ) flomap?
  (flomap-ht-append
   (hash-quote-flomap macro-stepper-hash-color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/32 height)))) 0)
   (step-flomap syntax-icon-color height material)))

(defproc (small-macro-stepper-flomap [height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
                                     [material deep-flomap-material-value? (default-icon-material)]
                                     ) flomap?
  (flomap-pin*
   0 0 7/16 0
   (step-flomap syntax-icon-color height material)
   (hash-quote-flomap small-macro-stepper-hash-color (* 3/4 height) material)))

(defproc (debugger-flomap [height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
                          [material deep-flomap-material-value? (default-icon-material)]
                          ) flomap?
  (flomap-ht-append
   (left-bomb-flomap metal-icon-color debugger-bomb-color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (step-flomap run-icon-color height material)))

(defproc (small-debugger-flomap [height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
                                [material deep-flomap-material-value? (default-icon-material)]
                                ) flomap?
  (flomap-pin*
   0 0 9/16 0
   (step-flomap run-icon-color height material)
   (left-bomb-flomap metal-icon-color debugger-bomb-color (* 3/4 height) material)))

(define-icon-wrappers
  ([height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
   [material deep-flomap-material-value? (default-icon-material)])
  [check-syntax-icon check-syntax-flomap]
  [small-check-syntax-icon small-check-syntax-flomap]
  [macro-stepper-icon macro-stepper-flomap]
  [small-macro-stepper-icon small-macro-stepper-flomap]
  [debugger-icon debugger-flomap]
  [small-debugger-icon small-debugger-flomap])
