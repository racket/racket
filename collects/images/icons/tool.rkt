#lang racket/base

(require racket/draw racket/class racket/math racket/sequence
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/renderfx.rkt"
         "../private/utils.rkt"
         "control.rkt"
         "misc.rkt"
         "style.rkt")

(provide (all-defined-out))

(define (check-syntax-icon-flomap [height  (toolbar-icon-height)]
                                  [material  (default-icon-material)])
  (flomap-ht-append
   (left-magnifying-glass-icon-flomap metal-icon-color syntax-icon-color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/32 height)))) 0)
   (check-icon-flomap run-icon-color height material)))

(define (small-check-syntax-icon-flomap [height  (toolbar-icon-height)]
                                        [material  (default-icon-material)])
  (flomap-pin*
   1 1 5/16 1
   (check-icon-flomap run-icon-color height material)
   (magnifying-glass-icon-flomap metal-icon-color syntax-icon-color (* 3/4 height) material)))

(define (macro-stepper-icon-flomap [height  (toolbar-icon-height)]
                                   [material  (default-icon-material)])
  (flomap-ht-append
   (text-icon-flomap "#'" (make-object font% 12 'system 'normal 'normal)
                     run-icon-color #t #t height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/32 height)))) 0)
   (step-icon-flomap (make-object color% 38 38 128) height material)))

(define (small-macro-stepper-icon-flomap [height  (toolbar-icon-height)]
                                         [material  (default-icon-material)])
  (flomap-pin*
   0 0 7/16 0
   (step-icon-flomap (make-object color% 38 38 128) height material)
   (text-icon-flomap "#'" (make-object font% 12 'system 'normal 'bold)
                     run-icon-color #t #t (* 3/4 height) material)))

(define (debugger-icon-flomap [height  (toolbar-icon-height)]
                              [material  (default-icon-material)])
  (flomap-ht-append
   (left-bomb-icon-flomap metal-icon-color halt-icon-color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (step-icon-flomap run-icon-color height material)))

(define (small-debugger-icon-flomap [height  (toolbar-icon-height)]
                                    [material  (default-icon-material)])
  (flomap-pin*
   0 0 9/16 0
   (step-icon-flomap run-icon-color height material)
   (left-bomb-icon-flomap metal-icon-color halt-icon-color (* 3/4 height) material)))

(define check-syntax-icon (compose flomap->bitmap check-syntax-icon-flomap))
(define small-check-syntax-icon (compose flomap->bitmap small-check-syntax-icon-flomap))
(define macro-stepper-icon (compose flomap->bitmap macro-stepper-icon-flomap))
(define small-macro-stepper-icon (compose flomap->bitmap small-macro-stepper-icon-flomap))
(define debugger-icon (compose flomap->bitmap debugger-icon-flomap))
(define small-debugger-icon (compose flomap->bitmap small-debugger-icon-flomap))
