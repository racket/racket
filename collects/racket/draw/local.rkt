#lang scheme/base
(require scheme/class)

(provide (all-defined-out))

(define-local-member-name
  ;; various
  adjust-lock

  ;; bitmap%
  get-cairo-surface
  get-cairo-alpha-surface

  ;; dc%
  in-cairo-context

  ;; region%
  install-region
  lock-region

  ;; font% and dc-backend<%>
  get-pango

  ;; font%
  get-ps-pango

  ;; dc-backend<%>
  get-cr
  end-cr
  reset-cr
  flush-cr
  init-cr-matrix
  get-font-metrics-key
  install-color
  dc-adjust-smoothing)
