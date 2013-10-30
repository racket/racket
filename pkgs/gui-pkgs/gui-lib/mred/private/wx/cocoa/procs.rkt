#lang racket/base
(require "../../syntax.rkt"
         racket/class
         racket/draw
         (only-in racket/draw/private/bitmap quartz-bitmap%)
         ffi/unsafe
         ffi/unsafe/objc
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "frame.rkt"
         "window.rkt"
         "finfo.rkt" ; file-creator-and-type
         "filedialog.rkt"
         "colordialog.rkt"
         "dc.rkt"
         "printer-dc.rkt"
         "menu-bar.rkt"
         "agl.rkt"
         "sound.rkt"
         "keycode.rkt"
         "../../lock.rkt"
         "../common/handlers.rkt"
         (except-in "../common/default-procs.rkt"
                    special-control-key
                    special-option-key
                    file-creator-and-type))


(provide
 (protect-out
  color-from-user-platform-mode
  font-from-user-platform-mode
  get-font-from-user
  find-graphical-system-path
  register-collecting-blit
  unregister-collecting-blit
  shortcut-visible-in-label?
  get-double-click-time
  get-control-font-face
  get-control-font-size
  get-control-font-size-in-pixels?
  cancel-quit
  display-origin
  display-size
  display-count
  bell
  hide-cursor
  get-display-depth
  is-color-display?
  id-to-menu-item
  can-show-print-setup?
  get-highlight-background-color
  get-highlight-text-color
  check-for-break)
 display-bitmap-resolution
 make-screen-bitmap
 make-gl-bitmap
 show-print-setup
 get-color-from-user
 get-panel-background
 fill-private-color
 flush-display
 play-sound
 file-creator-and-type
 file-selector
 key-symbol-to-menu-key
 needs-grow-box-spacer?
 get-current-mouse-state)

(import-class NSScreen NSCursor NSMenu NSEvent)

(define (find-graphical-system-path what)
  #f)

(define (color-from-user-platform-mode) "Show Picker")

(define-unimplemented get-font-from-user)
(define (font-from-user-platform-mode) #f)

(define (register-collecting-blit canvas x y w h on off on-x on-y off-x off-y)
  (send canvas register-collecting-blit x y w h on off on-x on-y off-x off-y))
(define (unregister-collecting-blit canvas)
  (send canvas unregister-collecting-blits))
(define (shortcut-visible-in-label? [x #f]) #f)

(define (get-double-click-time)
  500)
(define (get-control-font-face) "Lucida Grande")
(define (get-control-font-size) 13)
(define (get-control-font-size-in-pixels?) #f)
(define (cancel-quit) (void))

(define (check-for-break) #f)

(define (display-origin xb yb all? num fail)
  (if (or all? (positive? num))
      (unless (atomically
               (with-autorelease
                (let ([screens (tell NSScreen screens)])
                  (if (num . < . (tell #:type _NSUInteger screens count))
                      (let* ([screen (tell screens objectAtIndex: #:type _NSUInteger num)]
                             [f (if (zero? num)
                                    (tell #:type _NSRect screen visibleFrame)
                                    (tell #:type _NSRect screen frame))])
                        (set-box! xb ((if (and all? (zero? num)) + -) (->long (NSPoint-x (NSRect-origin f)))))
                        (unless (zero? num)
                          (let* ([screen0 (tell screens objectAtIndex: #:type _NSUInteger 0)]
                                 [f0 (tell #:type _NSRect screen0 frame)])
                            (set-box! yb (->long (- (+ (NSPoint-y (NSRect-origin f))
                                                       (NSSize-height (NSRect-size f)))
                                                    (NSSize-height (NSRect-size f0)))))))
                        #t)
                      #f))))
        (fail))
      (set-box! xb 0))
  (when (zero? num)
    (set-box! yb 0))
  (set-box! yb (+ (unbox yb) (get-menu-bar-height))))

(define (display-size xb yb all? num fail)
  (unless (atomically
           (with-autorelease
            (let ([screens (tell NSScreen screens)])
              (if (num . < . (tell #:type _NSUInteger screens count))
                  (let* ([screen (tell screens objectAtIndex: #:type _NSUInteger num)]
                         [f (if all?
                                (tell #:type _NSRect screen frame)
                                (tell #:type _NSRect screen visibleFrame))])
                    (set-box! xb (->long (NSSize-width (NSRect-size f))))
                    (set-box! yb (->long (- (NSSize-height (NSRect-size f))
                                            (cond
                                             [all? 0]
                                             [(positive? num) 0]
                                             [(tell #:type _BOOL NSMenu menuBarVisible) 0]
                                             ;; Make result consistent when menu bar is hidden:
                                             [else 
                                              (get-menu-bar-height)]))))
                    #t)
                  #f))))
    (fail)))


(define (display-count)
  (atomically
   (with-autorelease
    (let ([screens (tell NSScreen screens)])
      (tell #:type _NSUInteger screens count)))))

(define-appkit NSBeep (_fun -> _void))  
(define (bell) (NSBeep))

(define (hide-cursor) 
  (tellv NSCursor setHiddenUntilMouseMoves: #:type _BOOL #t))

(define (get-display-depth) 32)
(define (is-color-display?) #t)
(define (id-to-menu-item id) id)
(define (can-show-print-setup?) #t)

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

(define (needs-grow-box-spacer?)
  (not (version-10.7-or-later?)))

;; ------------------------------------------------------------
;; Mouse and modifier-key state

(define (get-current-mouse-state)
  (define posn (tell #:type _NSPoint NSEvent mouseLocation))
  (define buttons (if (version-10.6-or-later?)
                      (tell #:type _NSUInteger NSEvent pressedMouseButtons)
                      0))
  (define mods (tell #:type _NSUInteger NSEvent modifierFlags))
  (define (maybe v mask sym)
    (if (zero? (bitwise-and v mask))
        null
        (list sym)))
  (define h (let ([f (tell #:type _NSRect (tell NSScreen mainScreen) frame)])
              (NSSize-height (NSRect-size f))))
  (values (make-object point% 
                       (->long (NSPoint-x posn))
                       (->long (- (- h (NSPoint-y posn)) (get-menu-bar-height))))
          (append
           (maybe buttons #x1 'left)
           (maybe buttons #x2 'right)
           (maybe mods NSShiftKeyMask 'shift)
           (maybe mods NSCommandKeyMask 'meta)
           (maybe mods NSAlternateKeyMask 'alt)
           (maybe mods NSControlKeyMask 'control)
           (maybe mods NSAlphaShiftKeyMask 'caps))))
