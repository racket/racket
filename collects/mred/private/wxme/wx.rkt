#lang scheme/base
(require "../kernel.ss")

(define the-clipboard (get-the-clipboard))
(define the-x-selection-clipboard (get-the-x-selection))
(define the-brush-list (get-the-brush-list))
(define the-pen-list (get-the-pen-list))
(define the-font-list (get-the-font-list))
(define the-color-database (get-the-color-database))
(define the-font-name-directory (get-the-font-name-directory))

(define (family-symbol? s)
  (memq s '(default decorative roman script
             swiss modern symbol system)))
(define (style-symbol? s)
  (memq s '(normal italic slant)))
(define (weight-symbol? s)
  (memq s '(normal bold light)))
(define (smoothing-symbol? s)
  (memq s '(default smoothed unsmoothed partly-smoothed)))
(define (size? v) (and (exact-positive-integer? v)
                       (byte? v)))

(provide event%
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
         begin-refresh-sequence
         end-refresh-sequence
         begin-busy-cursor
         end-busy-cursor
         hide-cursor
         run-printout
         current-ps-setup
         family-symbol?
         style-symbol?
         weight-symbol?
         smoothing-symbol?)

(define (get-double-click-threshold)
  (get-double-click-time))
