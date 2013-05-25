#lang racket/base
(require racket/class)

(provide (all-defined-out))

;; editor-data%
(define-local-member-name
  get-s-dataclass
  get-s-next)

;; editor%
(define-local-member-name
  s-offscreen
  s-custom-cursor
  s-custom-cursor-overrides?
  s-keymap
  s-style-list
  get-s-style-list
  s-user-locked?
  s-modified?
  s-noundomode
  s-caret-snip
  s-inactive-caret-threshold
  s-filename
  s-need-on-display-size?
  really-can-edit?
  copy-out-x-selection
  own-x-selection
  do-own-x-selection
  perform-undo-list
  copy-ring-next
  begin-copy-buffer
  end-copy-buffer
  free-old-copies
  install-copy-buffer
  add-undo-rec
  read-snips-from-file
  admin-scroll-to
  do-buffer-paste
  insert-paste-snip
  insert-paste-string
  paste-region-data
  setting-admin
  init-new-admin
  do-read-insert
  do-set-caret-owner
  do-own-caret
  s-start-intercept
  s-end-intercept
  wait-sequence-lock
  begin-sequence-lock
  end-sequence-lock
  check-flow
  get-printing
  do-begin-print
  do-end-print
  do-has-print-page?)

;; text%
(define-local-member-name
  get-s-line-spacing
  get-s-last-snip
  get-s-total-width
  get-s-total-height
  get-s-snips
  refresh-box
  add-back-clickback
  do-insert-snips
  consistent-snip-lines)

;; editor-admin%
(define-local-member-name
  get-s-standard
  set-s-standard)

;; editor-canvas-editor-admin%
(define-local-member-name
  do-get-canvas
  do-scroll-to)

;; editor-stream-in%
(define-local-member-name
  set-s-read-format
  get-s-read-format
  set-s-read-version)

;; editor-snip%
(define-local-member-name
  do-set-graphics)

