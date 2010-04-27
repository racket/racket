#lang scheme/base
(require scheme/class)

(provide (all-defined-out))

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

;; editor-data%
(define-local-member-name
  get-s-dataclass
  get-s-next)

;; standard-snip-class-list%, editor-data-class-list%
(define-local-member-name
  reset-header-flags
  find-by-map-position)

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
  do-set-header-flag)

;; editor-stream-in%
(define-local-member-name
  set-s-read-format
  get-s-read-format
  set-s-read-version
  get-wxme-version)

;; editor-snip%
(define-local-member-name
  do-set-graphics)

