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

(defproc (check-syntax-flomap
          [#:height height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-ht-append
   (left-magnifying-glass-flomap #:frame-color metal-icon-color #:handle-color "chocolate"
                                 #:height height  #:material material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/32 height)))) 0)
   (check-flomap #:color syntax-icon-color #:height height #:material material)))

(defproc (small-check-syntax-flomap
          [#:height height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-pin*
   1 1 5/16 1
   (check-flomap #:color syntax-icon-color #:height height #:material material)
   (magnifying-glass-flomap #:frame-color metal-icon-color #:handle-color "chocolate"
                            #:height (* 3/4 height) #:material material)))

(defproc (macro-stepper-flomap
          [#:height height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-ht-append
   (hash-quote-flomap #:color macro-stepper-hash-color #:height height #:material material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/32 height)))) 0)
   (step-flomap #:color syntax-icon-color #:height height #:material material)))

(defproc (small-macro-stepper-flomap
          [#:height height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-pin*
   0 0 7/16 0
   (step-flomap #:color syntax-icon-color #:height height #:material material)
   (hash-quote-flomap #:color small-macro-stepper-hash-color
                      #:height (* 3/4 height) #:material material)))

(defproc (debugger-flomap
          [#:height height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-ht-append
   (left-bomb-flomap #:cap-color metal-icon-color #:bomb-color debugger-bomb-color
                     #:height height #:material material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (step-flomap #:color run-icon-color #:height height #:material material)))

(defproc (small-debugger-flomap
          [#:height height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-pin*
   0 0 9/16 0
   (step-flomap #:color run-icon-color #:height height #:material material)
   (left-bomb-flomap #:cap-color metal-icon-color #:bomb-color debugger-bomb-color
                     #:height (* 3/4 height) #:material material)))

(define-icon-wrappers
  ([#:height height (and/c rational? (>=/c 0)) (toolbar-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  [check-syntax-icon check-syntax-flomap]
  [small-check-syntax-icon small-check-syntax-flomap]
  [macro-stepper-icon macro-stepper-flomap]
  [small-macro-stepper-icon small-macro-stepper-flomap]
  [debugger-icon debugger-flomap]
  [small-debugger-icon small-debugger-flomap])
