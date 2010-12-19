#lang scheme/base
(require "../kernel.ss" "symbol-predicates.rkt")

(define the-clipboard (get-the-clipboard))
(define the-x-selection-clipboard (get-the-x-selection))

(define (size? v) (and (exact-positive-integer? v)
                       (byte? v)))

(provide (all-from-out "symbol-predicates.rkt")
         event%
         mouse-event%
         key-event%
         timer%
         canvas%
         bitmap-dc%
         color%
         the-color-database
         pen%
         the-pen-list
         brush%
         the-brush-list
         font%
         the-font-list
         the-font-name-directory
         cursor%
         bitmap%
         dc<%>
         post-script-dc%
         printer-dc%
         current-eventspace
         clipboard-client%
         clipboard<%>
         the-clipboard
         the-x-selection-clipboard
         get-double-click-threshold
         begin-busy-cursor
         end-busy-cursor
         hide-cursor
         run-printout
         current-ps-setup
         family-symbol?
         style-symbol?
         weight-symbol?
         smoothing-symbol?
         get-highlight-background-color
         get-highlight-text-color)

(define (get-double-click-threshold)
  (get-double-click-time))
