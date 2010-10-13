#lang racket/base
(require ffi/unsafe
         racket/class
	 "../../syntax.rkt"
	 "theme.rkt"
         "types.rkt"
         "utils.rkt"
         "const.rkt"
         "menu-item.rkt"
         "frame.rkt"
         "dc.rkt"
         "filedialog.rkt"
	 racket/draw)

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
 get-control-font-size-in-pixels?
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

(define (get-panel-background)
  (let ([c (GetSysColor COLOR_BTNFACE)])
    (make-object color% (GetRValue c) (GetGValue c) (GetBValue c))))

(define-unimplemented play-sound)
(define-unimplemented find-graphical-system-path)
(define (register-collecting-blit canvas x y w h on off on-x on-y off-x off-y)
  (send canvas register-collecting-blit x y w h on off on-x on-y off-x off-y))
(define (unregister-collecting-blit canvas)
  (send canvas unregister-collecting-blits))
(define (shortcut-visible-in-label? [? #f]) #t)
(define-unimplemented location->window)
(define-unimplemented send-event)
(define-unimplemented file-creator-and-type)
(define-unimplemented run-printout)
(define (get-double-click-time) 500)
(define (get-control-font-size) (get-theme-font-size))
(define (get-control-font-size-in-pixels?) #t)
(define-unimplemented cancel-quit)
(define-unimplemented fill-private-color)
(define (flush-display) (void))
(define-unimplemented write-resource)
(define-unimplemented get-resource)

(define-user32 MessageBeep (_wfun _UINT -> _BOOL))
(define (bell)
  (void (MessageBeep MB_OK)))

(define (hide-cursor) (void))

(define (get-display-depth) 32)

(define-unimplemented is-color-display?)
(define-unimplemented show-print-setup)
(define (can-show-print-setup?) #f)

(define (get-highlight-background-color)
  (let ([c (GetSysColor COLOR_HIGHLIGHT)])
    (make-object color% (GetRValue c) (GetGValue c) (GetBValue c))))
(define (get-highlight-text-color)
  (let ([c (GetSysColor COLOR_HIGHLIGHTTEXT)])
    (make-object color% (GetRValue c) (GetGValue c) (GetBValue c))))

(define/top (make-screen-bitmap [exact-positive-integer? w]
                                [exact-positive-integer? h])
  (make-object win32-bitmap% w h #f))

(define (check-for-break) #f)
