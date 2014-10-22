#lang racket/base
(require racket/class)

(provide (protect-out (all-defined-out)))

(define-local-member-name
  ;; various
  adjust-lock

  ;; bitmap%
  get-cairo-surface
  get-cairo-target-surface
  get-cairo-alpha-surface
  get-cairo-device-scale
  release-bitmap-storage
  get-bitmap-gl-context
  drop-alpha-s
  draw-bitmap-to

  ;; bitmap-dc%
  internal-get-bitmap
  internal-set-bitmap

  ;; dc%
  in-cairo-context
  get-clipping-matrix
  reset-config

  ;; region%
  install-region
  lock-region

  ;; font% and dc-backend<%>
  get-pango

  ;; font%
  get-ps-pango
  get-font-key

  ;; brush%
  get-surface-handle-info

  ;; dc-backend<%>
  get-cr
  release-cr
  end-cr
  reset-cr
  flush-cr
  init-cr-matrix
  init-effective-matrix
  get-font-metrics-key
  install-color
  dc-adjust-smoothing
  dc-adjust-cap-shape
  get-hairline-width
  can-combine-text?
  can-mask-bitmap?
  reset-clip
  get-clear-operator)
