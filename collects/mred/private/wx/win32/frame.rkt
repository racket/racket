#lang racket/base
(require racket/class
         (only-in racket/list last)
         ffi/unsafe
	 "../../syntax.rkt"
	 "../../lock.rkt"
	 "../common/queue.rkt"
	 "../common/freeze.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "theme.rkt"
	 "window.rkt"
         "wndclass.rkt"
         "cursor.rkt")

(provide frame%
         display-size
         display-origin)

(define-user32 SetLayeredWindowAttributes (_wfun _HWND _COLORREF _BYTE _DWORD -> _BOOL))
(define-user32 GetActiveWindow (_wfun -> _HWND))
(define-user32 SetFocus (_wfun _HWND -> _HWND))

(define-gdi32 GetDeviceCaps (_wfun _HDC _int -> _int))

(define-user32 DrawMenuBar (_wfun _HWND -> (r : _BOOL)
                                  -> (unless r (failed 'DrawMenuBar))))

(define-user32 IsZoomed (_wfun _HWND -> _BOOL))

(define-user32 SystemParametersInfoW (_wfun _UINT _UINT _pointer _UINT -> (r : _BOOL)
                                            -> (unless r (failed 'SystemParametersInfo))))
(define-cstruct _MINMAXINFO ([ptReserved _POINT]
                             [ptMaxSize _POINT]
                             [ptMaxPosition _POINT]
                             [ptMinTrackSize _POINT]
                             [ptMaxTrackSize _POINT]))

(define SPI_GETWORKAREA            #x0030)

(define (display-size xb yb ?)
  (atomically
   (let ([hdc (GetDC #f)])
     (set-box! xb (GetDeviceCaps hdc HORZRES))
     (set-box! yb (GetDeviceCaps hdc VERTRES))
     (ReleaseDC #f hdc))))

(define (display-origin xb yb avoid-bars?)
  (if avoid-bars?
      (let ([r (make-RECT 0 0 0 0)])
        (SystemParametersInfoW SPI_GETWORKAREA 0 r 0)
        (set-box! xb (RECT-left r))
        (set-box! yb (RECT-top r)))
      (begin
        (set-box! xb 0)
        (set-box! yb 0))))

(define mouse-frame #f)

(define WS_EX_TOOLWINDOW        #x00000080)
(define WS_EX_TOPMOST           #x00000008)
(define WS_EX_WINDOWEDGE        #x00000100)
(define WS_EX_PALETTEWINDOW     (bitwise-ior WS_EX_WINDOWEDGE 
                                             WS_EX_TOOLWINDOW 
                                             WS_EX_TOPMOST))

(defclass frame% window%
  (init parent
	label
	x y w h
	style)
  
  (inherit get-hwnd 
	   is-shown?
	   get-eventspace
           on-size
           get-size
           get-position
           pre-on-char pre-on-event
           reset-cursor-in-child)

  (define/public (create-frame parent label w h style)
    (CreateWindowExW (if (memq 'float style)
                         (bitwise-ior WS_EX_TOOLWINDOW
                                      (if (memq 'no-caption style)
                                          WS_EX_TOPMOST
                                          WS_EX_PALETTEWINDOW))
                         0)
                     "PLTFrame"
                     (if label label "")
                     (bitwise-ior
                      WS_POPUP
                      (if (memq 'no-resize-border style)
                          0
                          (bitwise-ior WS_THICKFRAME
                                       WS_BORDER
                                       WS_MAXIMIZEBOX))
                      (if (memq 'no-system-menu style)
                          0
                          WS_SYSMENU)
                      (if (memq 'no-caption style)
                          0
                          (bitwise-ior WS_CAPTION
                                       WS_MINIMIZEBOX)))
                     0 0 w h
                     #f
                     #f
                     hInstance
                     #f))

  (define saved-title (or label ""))
  (define hidden-zoomed? #f)

  (define min-width #f)
  (define min-height #f)
  (define max-width #f)
  (define max-height #f)

  (super-new [parent #f]
	     [hwnd (create-frame parent label w h style)]
	     [style (cons 'invisible style)])

  (define hwnd (get-hwnd))
  (SetLayeredWindowAttributes hwnd 0 255 LWA_ALPHA)

  (define/public (is-dialog?) #f)

  (define/override (show on?)
    (let ([es (get-eventspace)])
      (when (and on?
		 (eventspace-shutdown? es))
	(error (string->symbol
		(format "show method in ~a"
			(if (is-dialog?)
			    'dialog%
			    'frame%)))
	       "eventspace has been shutdown")))
    (super show on?))

  (define/override (direct-show on?)
    ;; atomic mode
    (when (eq? mouse-frame this) (set! mouse-frame #f))
    (register-frame-shown this on?)
    (when (and (not on?) (is-shown?))
      (set! hidden-zoomed? (is-maximized?)))
    (super direct-show on? (if hidden-zoomed?
                               SW_SHOWMAXIMIZED
                               SW_SHOW)))

  (define/private (stdret f d)
    (if (is-dialog?) d f))

  (define/override (wndproc w msg wParam lParam default)
    (cond
     [(= msg WM_CLOSE)
      (queue-window-event this (lambda () 
				 (when (on-close)
				   (direct-show #f))))
      0]
     [(= msg WM_SIZE)
      (unless (= wParam SIZE_MINIMIZED)
        (queue-window-event this (lambda () (on-size 0 0))))
      (stdret 0 1)]
     [(= msg WM_MOVE)
      (queue-window-event this (lambda () (on-size 0 0)))
      0]
     [(= msg WM_ACTIVATE)
      (let ([state (LOWORD wParam)]
            [minimized (HIWORD wParam)])
        (unless (not (zero? minimized))
          (let ([on? (or (= state WA_ACTIVE)
                         (= state WA_CLICKACTIVE))])
            (when on? (set-frame-focus))
            (queue-window-event this (lambda () (on-activate on?))))))
      0]
     [(and (= msg WM_COMMAND)
           (zero? (HIWORD wParam)))
      (queue-window-event this (lambda () (on-menu-command (LOWORD wParam))))
      0]
     [(= msg WM_INITMENU)
      (constrained-reply (get-eventspace)
                         (lambda () (on-menu-click))
                         (void))
      0]
     [(= msg WM_GETMINMAXINFO)
      (let ([mmi (cast lParam _LPARAM _MINMAXINFO-pointer)])
        (when (or max-width max-height)
          (set-MINMAXINFO-ptMaxTrackSize!
           mmi
           (make-POINT (or max-width
                           (POINT-x (MINMAXINFO-ptMaxTrackSize mmi)))
                       (or max-height
                           (POINT-y (MINMAXINFO-ptMaxTrackSize mmi))))))
        (when (or min-width min-height)
          (set-MINMAXINFO-ptMinTrackSize!
           mmi
           (make-POINT (or min-width
                           (POINT-x (MINMAXINFO-ptMinTrackSize mmi)))
                       (or min-height
                           (POINT-y (MINMAXINFO-ptMinTrackSize mmi)))))))
      0]
     [else (super wndproc w msg wParam lParam default)]))

  (define/override (set-size x y w h)
    (unless (and (= w -1) (= h -1))
      (maximize #f))
    (super set-size x y w h))

  (define/public (on-close) (void))

  (define/override (is-shown-to-root?)
    (is-shown?))
  (define/override (is-enabled-to-root?)
    #t)

  (define/override (get-x)
    (RECT-left (GetWindowRect hwnd)))
  (define/override (get-y)
    (RECT-top (GetWindowRect hwnd)))

  (def/public-unimplemented on-toolbar-click)
  (def/public-unimplemented on-menu-click)

  (define/public (on-menu-command i) (void))

  (def/public-unimplemented on-mdi-activate)

  (define/public (enforce-size min-x min-y max-x max-y step-x step-y)
    (set! min-width (max 1 min-x))
    (set! min-height (max 1 min-y))
    (set! max-width (and (positive? max-x) max-x))
    (set! max-height (and (positive? max-y) max-y)))

  (define focus-window-path #f)
  (define/override (not-focus-child v)
    (when (and focus-window-path
               (memq v focus-window-path))
      (set! focus-window-path #f)))
  (define/override (set-top-focus win win-path child-hwnd)
    (set! focus-window-path win-path)
    (when (ptr-equal? hwnd (GetActiveWindow))
      (SetFocus child-hwnd)))

  (define/private (set-frame-focus)
    (when (pair? focus-window-path)
      (SetFocus (send (last focus-window-path) get-focus-hwnd))))

  (define/override (can-accept-focus?)
    #f)
  (define/override (child-can-accept-focus?)
    #t)

  (define/public (on-activate on?) (void))

  (define/override (call-pre-on-event w e)
    (pre-on-event w e))
  (define/override (call-pre-on-char w e)
    (pre-on-char w e))

  (define/override (generate-parent-mouse-ins mk)
    ;; assert: in-window is always the panel child
    (unless (eq? mouse-frame this)
      (when mouse-frame
        (let ([win mouse-frame])
          (set! mouse-frame #f)
          (send win send-leaves mk)))
      (set! mouse-frame this))
    #f)

  (define/override (send-child-leaves mk)
    (if (eq? mouse-frame this)
        (if saved-child
            (send saved-child send-leaves mk)
            #f)
        #f))

  (define/override (reset-cursor default)
    (if wait-cursor-on?
        (void (SetCursor (get-wait-cursor)))
        (when saved-child
          (reset-cursor-in-child saved-child default))))

  (define/override (get-dialog-level) 0)

  (define/public (frame-relative-dialog-status win) 
    #f)

  (define wait-cursor-on? #f)
  (define/public (set-wait-cursor-mode on?)
    (set! wait-cursor-on? on?)
    (when (eq? mouse-frame this)
      (if on?
          (void (SetCursor (get-wait-cursor)))
          (reset-cursor (get-arrow-cursor)))))
  (define/public (is-wait-cursor-on?)
    wait-cursor-on?)

  (define/override (center mode wrt)
    (let ([sw (box 0)]
          [sh (box 0)]
          [w (box 0)]
          [h (box 0)]
          [x (box 0)]
          [y (box 0)])
      (display-size sw sh #f)
      (get-size w h)
      (MoveWindow hwnd
                  (if (or (eq? mode 'both)
                          (eq? mode 'horizontal))
                      (quotient (- (unbox sw) (unbox w)) 2)
                      (get-x))
                  (if (or (eq? mode 'both)
                          (eq? mode 'vertical))
                      (quotient (- (unbox sh) (unbox h)) 2)
                      (get-x))
                  (unbox w)
                  (unbox h)
                  #t)))

  (define saved-child #f)
  (define/override (register-child child on?)
    (unless on? (error 'register-child-in-frame "did not expect #f"))
    (unless (or (not saved-child) (eq? child saved-child))
      (error 'register-child-in-frame "expected only one child"))
    (set! saved-child child)
    (send child set-arrow-cursor))
  (define/override (register-child-in-parent on?)
    (void))

  (define/override (get-top-frame) this)

  (def/public-unimplemented designate-root-frame)
  (def/public-unimplemented system-menu)

  (define modified? #f)
  (define/public (set-modified on?)
    (unless (eq? modified? (and on? #t))
      (set! modified? (and on? #t))
      (set-title saved-title)))

  (define/public (is-maximized?)
    (if (is-shown?)
        hidden-zoomed?
        (IsZoomed hwnd)))
  
  (define/public (maximize on?)
    (if (is-shown?)
        (ShowWindow hwnd (if on?
                             SW_MAXIMIZE
                             SW_RESTORE))
        (set! hidden-zoomed? (and on? #t))))
    
  (def/public-unimplemented iconized?)
  (def/public-unimplemented get-menu-bar)

  (define menu-bar #f)
  (define/public (set-menu-bar mb)
    (atomically
     (set! menu-bar mb)
     (send mb set-parent this)))

  (define/public (draw-menu-bar)
    (DrawMenuBar hwnd))
  
  (define/override (is-frame?) #t)

  (define/public (set-icon bm mask [mode 'both])
    (void))

  (def/public-unimplemented iconize)
  (define/public (set-title s)
    (atomically
     (set! saved-title s)
     (SetWindowTextW (get-hwnd) (string-append s (if modified? "*" ""))))))


