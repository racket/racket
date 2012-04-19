#lang racket/base
(require racket/class)

(provide (all-defined-out))

;; standard-snip-class-list%, editor-data-class-list%
(define-local-member-name
  reset-header-flags
  find-by-map-position)

;; snip% and editor%
(define-local-member-name
  s-admin)

;; snip%
(define-local-member-name
  s-prev set-s-prev
  s-next set-s-next
  s-count
  s-style set-s-style
  s-line set-s-line
  s-snipclass set-s-snipclass
  s-flags set-s-flags
  s-dtext get-s-dtext
  s-buffer get-s-buffer
  str-w set-str-w
  s-set-flags
  do-copy-to)

;; string-snip%
(define-local-member-name
  insert-with-offset)

;; snip-class%
(define-local-member-name
  get-s-required?
  s-read)

;; editor-stream%
(define-local-member-name
  get-sl
  get-dl
  set-sl
  set-dl
  add-sl
  add-dl
  set-s-sll
  get-s-sll
  get-s-scl
  get-s-bdl
  get-s-style-count
  set-s-style-count
  do-reading-version
  do-map-position
  do-get-header-flag
  do-set-header-flag
  get-wxme-version)
