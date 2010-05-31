#lang scheme/base
(require "../../syntax.rkt")

(provide
 special-control-key
 special-option-key
 application-file-handler
 application-quit-handler
 application-about-handler
 application-pref-handler
 get-color-from-user
 get-font-from-user
 get-panel-background
 play-sound
 check-for-break
 find-graphical-system-path
 get-top-level-windows
 register-collecting-blit
 unregister-collecting-blit
 shortcut-visible-in-label?
 in-atomic-region
 set-menu-tester
 location->window
 set-dialogs
 set-executer
 send-event
 file-creator-and-type
 begin-refresh-sequence
 end-refresh-sequence
 run-printout
 get-double-click-time
 set-combo-box-font
 draw-tab
 draw-tab-base
 key-symbol-to-integer
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
 end-busy-cursor
 is-busy?
 begin-busy-cursor
 get-display-depth
 is-color-display?
 file-selector
 id-to-menu-item
 get-the-x-selection
 get-the-clipboard
 show-print-setup
 can-show-print-setup?)


(define-unimplemented special-control-key)
(define-unimplemented special-option-key)
(define-unimplemented application-file-handler)
(define-unimplemented application-quit-handler)
(define-unimplemented application-about-handler)
(define-unimplemented application-pref-handler)
(define-unimplemented get-color-from-user)
(define-unimplemented get-font-from-user)
(define-unimplemented get-panel-background)
(define-unimplemented play-sound)
(define-unimplemented check-for-break)
(define-unimplemented find-graphical-system-path)
(define-unimplemented get-top-level-windows)
(define-unimplemented register-collecting-blit)
(define-unimplemented unregister-collecting-blit)
(define-unimplemented shortcut-visible-in-label?)
(define-unimplemented in-atomic-region)
(define-unimplemented set-menu-tester)
(define-unimplemented location->window)
(define-unimplemented set-dialogs)
(define-unimplemented set-executer)
(define-unimplemented send-event)
(define-unimplemented file-creator-and-type)
(define-unimplemented begin-refresh-sequence)
(define-unimplemented end-refresh-sequence)
(define-unimplemented run-printout)
(define-unimplemented get-double-click-time)
(define-unimplemented set-combo-box-font)
(define-unimplemented draw-tab)
(define-unimplemented draw-tab-base)
(define-unimplemented key-symbol-to-integer)
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