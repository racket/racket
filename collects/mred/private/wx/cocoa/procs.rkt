#lang racket/base
(require "../../syntax.rkt"
         racket/class
         racket/draw
         ffi/unsafe
         ffi/unsafe/objc
         "utils.rkt"
         "types.rkt"
         "frame.rkt"
         "window.rkt"
         "finfo.rkt" ; file-creator-and-type
         "filedialog.rkt"
         "dc.rkt"
         "printer-dc.rkt"
         "../common/printer.rkt"
         "menu-bar.rkt"
         "agl.rkt"
         "../../lock.rkt"
         "../common/handlers.rkt")

(provide
 application-file-handler
 application-quit-handler
 application-about-handler
 application-pref-handler
 get-color-from-user
 get-font-from-user
 get-panel-background
 play-sound
 find-graphical-system-path
 register-collecting-blit
 unregister-collecting-blit
 shortcut-visible-in-label?
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
 make-gl-bitmap
 check-for-break)

(import-class NSScreen NSCursor)


(define-unimplemented get-color-from-user)
(define-unimplemented get-font-from-user)
(define (get-panel-background) (make-object color% "gray"))
(define-unimplemented play-sound)
(define-unimplemented find-graphical-system-path)
(define (register-collecting-blit canvas x y w h on off on-x on-y off-x off-y)
  (send canvas register-collecting-blit x y w h on off on-x on-y off-x off-y))
(define (unregister-collecting-blit canvas)
  (send canvas unregister-collecting-blits))
(define (shortcut-visible-in-label? [x #f]) #f)
(define-unimplemented send-event)
(define (begin-refresh-sequence) (void))
(define (end-refresh-sequence) (void))

(define run-printout (make-run-printout printer-dc%))

(define (get-double-click-time)
  500)
(define (get-control-font-size) 13)
(define (get-control-font-size-in-pixels?) #f)
(define (cancel-quit) (void))
(define-unimplemented fill-private-color)
(define-unimplemented write-resource)
(define-unimplemented get-resource)

(define (check-for-break) #f)

(define (display-origin xb yb all?)
  (set-box! xb 0)
  (if all?
      (set-box! yb 0)
      (set-box! yb (get-menu-bar-height))))
(define (display-size xb yb all?)
  (atomically
   (with-autorelease
    (let* ([screen (tell (tell NSScreen screens) objectAtIndex: #:type _NSUInteger 0)]
           [f (if all?
                  (tell #:type _NSRect screen frame)
                  (tell #:type _NSRect screen visibleFrame))])
      (set-box! xb (->long (NSSize-width (NSRect-size f))))
      (set-box! yb (->long (NSSize-height (NSRect-size f))))))))

(define-appkit NSBeep (_fun -> _void))  
(define (bell) (NSBeep))

(define (hide-cursor) 
  (tellv NSCursor setHiddenUntilMouseMoves: #:type _BOOL #t))

(define (get-display-depth) 32)
(define-unimplemented is-color-display?)
(define (id-to-menu-item id) id)
(define (can-show-print-setup?) #t)

(define/top (make-screen-bitmap [exact-positive-integer? w]
                                [exact-positive-integer? h])
  (make-object quartz-bitmap% w h))

(define/top (make-gl-bitmap [exact-positive-integer? w]
                            [exact-positive-integer? h]
                            [gl-config% c])
  (create-gl-bitmap w h c))

;; ------------------------------------------------------------
;; Text & highlight color

(import-class NSColor)

(define-cocoa NSDeviceRGBColorSpace _id)

(define (get-highlight-background-color)
  (let ([hi (as-objc-allocation-with-retain
             (tell (tell NSColor selectedTextBackgroundColor) 
                   colorUsingColorSpaceName: NSDeviceRGBColorSpace))]
        [as-color (lambda (v)
                    (inexact->exact (floor (* 255.0 v))))])
    (begin0
     (make-object color%
                  (as-color
                   (tell #:type _CGFloat hi redComponent))
                  (as-color
                   (tell #:type _CGFloat hi greenComponent))
                  (as-color
                   (tell #:type _CGFloat hi blueComponent)))
     (release hi))))

(define (get-highlight-text-color)
  #f)
