#lang scheme/base
(require "../../syntax.rkt")

(provide
 special-control-key
 special-option-key
 get-color-from-user
 get-font-from-user
 get-panel-background
 play-sound
 find-graphical-system-path
 register-collecting-blit
 unregister-collecting-blit
 shortcut-visible-in-label?
 location->window
 send-event
 file-creator-and-type
 run-printout
 get-double-click-time
 get-control-font-size
 cancel-quit
 fill-private-color
 flush-display
 write-resource
 get-resource
 display-origin
 display-size
 bell
 hide-cursor
 get-display-depth
 is-color-display?
 file-selector
 id-to-menu-item
 get-the-x-selection
 get-the-clipboard
 show-print-setup
 can-show-print-setup?
 get-highlight-background-color
 get-highlight-text-color
 make-screen-bitmap
 check-for-break)

(define-unimplemented special-control-key)
(define-unimplemented special-option-key)
(define-unimplemented get-color-from-user)
(define-unimplemented get-font-from-user)
(define-unimplemented get-panel-background)
(define-unimplemented play-sound)
(define-unimplemented find-graphical-system-path)
(define-unimplemented register-collecting-blit)
(define-unimplemented unregister-collecting-blit)
(define-unimplemented shortcut-visible-in-label?)
(define-unimplemented location->window)
(define-unimplemented send-event)
(define-unimplemented file-creator-and-type)
(define-unimplemented run-printout)
(define-unimplemented get-double-click-time)
(define-unimplemented get-control-font-size)
(define-unimplemented cancel-quit)
(define-unimplemented fill-private-color)
(define-unimplemented flush-display)
(define-unimplemented write-resource)
(define-unimplemented get-resource)
(define-unimplemented display-origin)
(define-unimplemented display-size)
(define-unimplemented bell)
(define-unimplemented hide-cursor)
(define-unimplemented end-busy-cursor)
(define-unimplemented is-busy?)
(define-unimplemented begin-busy-cursor)
(define-unimplemented get-display-depth)
(define-unimplemented is-color-display?)
(define-unimplemented file-selector)
(define-unimplemented id-to-menu-item)
(define-unimplemented get-the-x-selection)
(define-unimplemented get-the-clipboard)
(define-unimplemented show-print-setup)
(define-unimplemented can-show-print-setup?)
(define-unimplemented get-highlight-background-color)
(define-unimplemented get-highlight-text-color)
(define-unimplemented make-screen-bitmap)

(define (check-for-break) #f)
