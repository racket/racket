#lang racket/base
(require racket/class
         ffi/unsafe
         racket/draw
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/canvas-mixin.rkt"
         "../common/backing-dc.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "wndclass.rkt"
         "window.rkt"
         "dc.rkt")

(provide canvas%)

(define-user32 GetDC (_wfun _HWND -> _HDC))
(define-user32 BeginPaint (_wfun _HWND _pointer -> _HDC))
(define-user32 EndPaint (_wfun _HDC _pointer -> _BOOL))
(define-user32 InvalidateRect (_wfun _HWND (_or-null _RECT-pointer) _BOOL -> _BOOL))

(define canvas% 
  (canvas-mixin
   (class window%
     (init parent
           x y w h
           style
           [ignored-name #f]
           [gl-config #f])

     (inherit get-win32
              get-client-size)

     (define hscroll? (memq 'hscroll style))
     (define vscroll? (memq 'vscroll style))

     (super-new [parent parent]
                [win32
                 (CreateWindowExW 0
                                  "PLTCanvas"
                                  #f
                                  (bitwise-ior WS_CHILD WS_VISIBLE
                                               (if hscroll? WS_HSCROLL 0)
                                               (if vscroll? WS_VSCROLL 0))
                                  0 0 w h
                                  (send parent get-win32)
                                  #f
                                  hInstance
                                  #f)]
                [style style])

     (define win32 (get-win32))

     (define/override (wndproc w msg wparam lparam)
       (cond
        [(= msg WM_PAINT)
         (let* ([ps (malloc 128)]
                [hdc (BeginPaint w ps)])
           (unless (positive? paint-suspended)
             (unless (do-backing-flush this dc hdc)
               (queue-paint))
             (do-backing-flush this dc hdc))
           (EndPaint hdc ps))
         0]
        [else (super wndproc w msg wparam lparam)]))
     
     (define dc (new dc% [canvas this]))

     (define/public (get-dc) dc)

     ;; The `queue-paint' and `paint-children' methods
     ;; are defined by `canvas-mixin' from ../common/canvas-mixin
     (define/public (queue-paint) (void))
     (define/public (request-canvas-flush-delay)
       (request-flush-delay this))
     (define/public (cancel-canvas-flush-delay req)
       (cancel-flush-delay req))
     (define/public (queue-canvas-refresh-event thunk)
       (queue-window-refresh-event this thunk))

     (define/public (get-flush-window) win32)

     (define/public (begin-refresh-sequence)
       (send dc suspend-flush))
     (define/public (end-refresh-sequence)
       (send dc resume-flush))

     (define/public (on-paint) (void))
     (define/override (refresh) (queue-paint))

     (define/public (queue-backing-flush)
       (void (InvalidateRect win32 #f #t)))

     (define/public (make-compatible-bitmap w h)
       (send dc make-backing-bitmap w h))

     (define paint-suspended 0)
     (define/public (suspend-paint-handling)
       (atomically
        (set! paint-suspended (add1 paint-suspended))))
     (define/public (resume-paint-handling)
       (atomically
        (unless (zero? paint-suspended)
          (set! paint-suspended (sub1 paint-suspended)))))

     (define/public (get-virtual-size w h)
       (get-client-size w h))

     (define transparent? (memq 'transparent style))
     (define bg-col (make-object color% "white"))
     (define/public (get-canvas-background) (if transparent?
                                                #f
                                                bg-col))
     (define/public (set-canvas-background col) (set! bg-col col))

     (def/public-unimplemented set-background-to-gray)
     (def/public-unimplemented on-scroll)
     (def/public-unimplemented set-scroll-page)
     (def/public-unimplemented set-scroll-range)
     (def/public-unimplemented set-scroll-pos)
     (def/public-unimplemented get-scroll-page)
     (def/public-unimplemented get-scroll-range)
     (def/public-unimplemented get-scroll-pos)
     (def/public-unimplemented scroll)
     (def/public-unimplemented warp-pointer)
     (def/public-unimplemented view-start)
     (def/public-unimplemented set-resize-corner)
     (def/public-unimplemented show-scrollbars)
     (def/public-unimplemented set-scrollbars)
     (def/public-unimplemented on-char)
     (def/public-unimplemented on-event))))
