#lang racket/base
(require racket/class)

(provide (protect-out (all-defined-out)))

(define-local-member-name
  ;; clipboard-client%:
  get-client-eventspace
  set-client-eventspace

  ;; cursor%
  get-driver)

