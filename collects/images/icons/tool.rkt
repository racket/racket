#lang racket/base

(require racket/draw racket/class racket/math racket/sequence
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "control.rkt"
         "misc.rkt"
         "style.rkt")

(provide (all-defined-out))

(define debugger-bomb-color (make-object color% 128 32 32))
(define macro-stepper-hash-color (make-object color% 30 96 30))

(define (check-syntax-flomap [height (toolbar-icon-height)] [material (default-icon-material)])
  (flomap-ht-append
   (left-magnifying-glass-flomap metal-icon-color "chocolate" height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/32 height)))) 0)
   (check-flomap syntax-icon-color height material)))

(define (small-check-syntax-flomap [height (toolbar-icon-height)] [material (default-icon-material)])
  (flomap-pin*
   1 1 5/16 1
   (check-flomap syntax-icon-color height material)
   (magnifying-glass-flomap metal-icon-color "chocolate" (* 3/4 height) material)))

(define (macro-stepper-flomap [height (toolbar-icon-height)] [material (default-icon-material)])
  (flomap-ht-append
   (text-flomap "#'" (make-object font% 12 'system 'normal 'normal)
                macro-stepper-hash-color #t #t height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/32 height)))) 0)
   (step-flomap syntax-icon-color height material)))

(define (small-macro-stepper-flomap [height (toolbar-icon-height)] [material (default-icon-material)])
  (flomap-pin*
   0 0 7/16 0
   (step-flomap syntax-icon-color height material)
   (text-flomap "#'" (make-object font% 12 'system 'normal 'bold)
                macro-stepper-hash-color #t #t (* 3/4 height) material)))

(define (debugger-flomap [height (toolbar-icon-height)] [material (default-icon-material)])
  (flomap-ht-append
   (left-bomb-flomap metal-icon-color debugger-bomb-color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (step-flomap run-icon-color height material)))

(define (small-debugger-flomap [height (toolbar-icon-height)] [material (default-icon-material)])
  (flomap-pin*
   0 0 9/16 0
   (step-flomap run-icon-color height material)
   (left-bomb-flomap metal-icon-color debugger-bomb-color (* 3/4 height) material)))

(define check-syntax-icon (compose flomap->bitmap check-syntax-flomap))
(define small-check-syntax-icon (compose flomap->bitmap small-check-syntax-flomap))
(define macro-stepper-icon (compose flomap->bitmap macro-stepper-flomap))
(define small-macro-stepper-icon (compose flomap->bitmap small-macro-stepper-flomap))
(define debugger-icon (compose flomap->bitmap debugger-flomap))
(define small-debugger-icon (compose flomap->bitmap small-debugger-flomap))
