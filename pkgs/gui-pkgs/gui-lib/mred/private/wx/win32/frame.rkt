#lang racket/base
(require racket/class
         racket/draw
         (only-in racket/list last)
         ffi/unsafe
         ffi/unsafe/alloc
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
         "hbitmap.rkt"
         "cursor.rkt"
	 "menu-item.rkt")

(provide 
 (protect-out frame%
              display-size
              display-origin
	      display-count
              display-bitmap-resolution))

(define-user32 SetLayeredWindowAttributes (_wfun _HWND _COLORREF _BYTE _DWORD -> _BOOL))
(define-user32 GetActiveWindow (_wfun -> _HWND))
(define-user32 SetFocus (_wfun _HWND -> _HWND))
(define-user32 BringWindowToTop (_wfun _HWND -> (r : _BOOL) -> (unless r (failed 'BringWindowToTop))))

(define-gdi32 GetDeviceCaps (_wfun _HDC _int -> _int))

(define-user32 DrawMenuBar (_wfun _HWND -> (r : _BOOL)
                                  -> (unless r (failed 'DrawMenuBar))))

(define-user32 IsZoomed (_wfun _HWND -> _BOOL))

(define-user32 EnumDisplayMonitors (_wfun _HDC 
					  _pointer 
					  (_wfun #:atomic? #t _pointer _HDC _RECT-pointer _pointer 
						 -> _BOOL)
					  _pointer -> _BOOL))

(define-user32 SystemParametersInfoW (_wfun _UINT _UINT _pointer _UINT -> (r : _BOOL)
                                            -> (unless r (failed 'SystemParametersInfo))))
(define-cstruct _MINMAXINFO ([ptReserved _POINT]
                             [ptMaxSize _POINT]
                             [ptMaxPosition _POINT]
                             [ptMinTrackSize _POINT]
                             [ptMaxTrackSize _POINT]))

(define-cstruct _ICONINFO ([fIcon _BOOL]
                           [xHotspot _DWORD]
                           [yHotspot _DWORD]
                           [hbmMask _HBITMAP]
                           [hbmColor _HBITMAP]))

(define-user32 DestroyIcon (_wfun _HICON -> (r : _BOOL)
                                  -> (unless r (failed 'DestroyIcon)))
  #:wrap (deallocator))
(define-user32 CreateIconIndirect (_wfun _ICONINFO-pointer -> (r : _HICON)
                                         -> (or r (failed 'CreateIconIndirect)))
  #:wrap (allocator DestroyIcon))

(define-cstruct _MONITORINFO ([cbSize _DWORD]
			      [rcMonitor _RECT]
			      [rcWork _RECT]
			      [dwFlags _DWORD]))

(define-user32 GetMonitorInfoW (_wfun _pointer _MONITORINFO-pointer 
				      -> (r : _BOOL)
				      -> (unless r (failed 'GetMonitorInfoW))))

(define SPI_GETWORKAREA            #x0030)

(define MA_NOACTIVATEANDEAT 4)

(define MONITORINFOF_PRIMARY 1)

(define (get-all-screen-rects)
  (let ([rects null]
	[pos 0])
    (EnumDisplayMonitors #f #f (lambda (mon dc r ptr)
				 (define mi (cast (malloc _MONITORINFO)
						  _pointer
						  _MONITORINFO-pointer))
				 (set-MONITORINFO-cbSize! mi (ctype-sizeof _MONITORINFO))
				 (GetMonitorInfoW mon mi)
				 (set! pos (add1 pos))
				 (set! rects (cons
					      (list*
					       ;; sort first by main monitor:
					       (positive?
						(bitwise-and MONITORINFOF_PRIMARY
							     (MONITORINFO-dwFlags mi)))
					       ;; otherwise, preserve order:
					       pos
					       ;; monitor rectangle, which is the goal:
					       (list (RECT-left r)
						     (RECT-top r)
						     (RECT-right r)
						     (RECT-bottom r)))
					      rects))
				 #t)
			 #f)
    (map
     cddr
     (sort rects (lambda (a b)
		   (cond
		    [(and (car a) (not (car b))) #t]
		    [(and (car b) (not (car a))) #f]
		    [else (< (cadr a) (cadr b))]))))))

(define (display-size xb yb all? num fail)
  (cond
   [(positive? num)
    (let ([rs (get-all-screen-rects)])
      (unless (num . < . (length rs))
        (fail))
      (let ([r (list-ref rs num)])
	(set-box! xb (- (caddr r) (car r)))
	(set-box! yb (- (cadddr r) (cadr r)))))]
   [all?
    (atomically
     (let ([hdc (GetDC #f)])
       (set-box! xb (GetDeviceCaps hdc HORZRES))
       (set-box! yb (GetDeviceCaps hdc VERTRES))
       (ReleaseDC #f hdc)))]
   [else
    (let ([r (make-RECT 0 0 0 0)])
      (SystemParametersInfoW SPI_GETWORKAREA 0 r 0)
      (set-box! xb (- (RECT-right r) (RECT-left r)))
      (set-box! yb (- (RECT-bottom r) (RECT-top r))))]))

(define (display-origin xb yb avoid-bars? num fail)
  (cond
   [(positive? num)
    (let ([rs (get-all-screen-rects)])
      (unless (num . < . (length rs))
	(fail))
      (let ([r (list-ref rs num)])
	(set-box! xb (- (car r)))
	(set-box! yb (- (cadr r)))))]
   [avoid-bars?
    (let ([r (make-RECT 0 0 0 0)])
      (SystemParametersInfoW SPI_GETWORKAREA 0 r 0)
      (set-box! xb (RECT-left r))
      (set-box! yb (RECT-top r)))]
   [else
    (set-box! xb 0)
    (set-box! yb 0)]))

(define (display-count)
  (let ([pos 0])
    (EnumDisplayMonitors #f #f (lambda (mon dc r ptr)
                                 (set! pos (add1 pos))
                                 #t)
                         #f)
    pos))

(define (display-bitmap-resolution num fail)
  (if (or (zero? num)
          (num . < . (display-count)))
      1.0
      (fail)))

(define mouse-frame #f)

(define WS_EX_TOOLWINDOW        #x00000080)
(define WS_EX_TOPMOST           #x00000008)
(define WS_EX_WINDOWEDGE        #x00000100)
(define WS_EX_PALETTEWINDOW     (bitwise-ior WS_EX_WINDOWEDGE 
                                             WS_EX_TOOLWINDOW 
                                             WS_EX_TOPMOST))

(define-cstruct _WINDOWPLACEMENT
  ([length _UINT]
   [flags _UINT]
   [showCmd _UINT]
   [ptMinPosition _POINT]
   [ptMaxPosition _POINT]
   [rcNormalPosition _RECT]))

(define-user32 GetWindowPlacement (_wfun _HWND _WINDOWPLACEMENT-pointer -> (r : _BOOL)
                                         -> (unless r (failed 'GetWindowPlacement))))

(define-user32 IsIconic (_fun _HWND -> _BOOL))

(defclass frame% window%
  (init parent
	label
	x y w h
	style)
  
  (inherit get-hwnd 
	   is-shown?
	   get-eventspace
           queue-on-size
           pre-on-char pre-on-event
           reset-cursor-in-child)

  (define/public (create-frame parent label x y w h style)
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
                     (or x CW_USEDEFAULT)
                     (or y CW_USEDEFAULT)
                     w h
                     #f
                     #f
                     hInstance
                     #f))

  (define saved-title (or label ""))
  (define hidden-zoomed? #f)
  (define float-without-caption? (and (memq 'float style)
                                      (memq 'no-caption style)))

  (define min-width #f)
  (define min-height #f)
  (define max-width #f)
  (define max-height #f)

  (super-new [parent #f]
	     [hwnd (create-frame parent label x y w h style)]
	     [style (cons 'deleted style)])

  (define hwnd (get-hwnd))
  (SetLayeredWindowAttributes hwnd 0 255 LWA_ALPHA)

  ;; record delta between size and client size
  ;; for getting the client size when the frame 
  ;; is iconized:
  (define-values (client-dw client-dh)
    (let ([w (box 0)] [h (box 0)]
          [cw (box 0)] [ch (box 0)])
      (get-size w h)
      (get-client-size cw ch)
      (values (- (unbox w) (unbox cw)) 
              (- (unbox h) (unbox  ch)))))

  (define/public (is-dialog?) #f)

  (define/override (show on?)
    (let ([es (get-eventspace)])
      (when on?
        (when (eventspace-shutdown? es)
          (error (string->symbol
                  (format "show method in ~a"
                          (if (is-dialog?)
                              'dialog%
                              'frame%)))
                 "eventspace has been shutdown"))
        (when saved-child
          (if (eq? (current-thread) (eventspace-handler-thread es))
              (do-paint-children)
              (let ([s (make-semaphore)])
                (queue-callback (lambda ()
                                  (do-paint-children)
                                  (semaphore-post s)))
                (sync/timeout 1 s))))))
    ;; calling `direct-show' instead of `show' avoids
    ;;  calling `show-children':
    (atomically (direct-show on?)))

  (define/private (do-paint-children)
    (when saved-child
      (send saved-child paint-children)))

  (define/override (direct-show on?)
    ;; atomic mode
    (when (eq? mouse-frame this) (set! mouse-frame #f))
    (register-frame-shown this on?)
    (when (and (not on?) (is-shown?))
      (set! hidden-zoomed? (is-maximized?)))
    (super direct-show on? (if hidden-zoomed?
                               SW_SHOWMAXIMIZED
                               (if float-without-caption?
                                   SW_SHOWNOACTIVATE
                                   SW_SHOW)))
    (when (and on? (iconized?))
      (ShowWindow hwnd SW_RESTORE))
    (when on?
      (unless float-without-caption?
        (BringWindowToTop hwnd))))

  (define/public (destroy)
    (direct-show #f))

  (define/private (stdret f d)
    (if (is-dialog?) d f))

  (define/override (wndproc w msg wParam lParam default)
    (cond
     [(= msg WM_CLOSE)
      (unless (other-modal? this)
        (queue-window-event this (lambda () 
                                   (when (on-close)
                                     (atomically
                                      (direct-show #f))))))
      0]
     [(and (= msg WM_SIZE)
           (not (= wParam SIZE_MINIMIZED)))
      (queue-window-event this (lambda () (queue-on-size)))
      ;; for live resize:
      (constrained-reply (get-eventspace)
			 (lambda ()
			   (let loop () (pre-event-sync #t) (when (yield) (loop))))
			 (void))
      (stdret 0 1)]
     [(= msg WM_MOVE)
      (unless (iconized?)
        (queue-window-event this (lambda () (queue-on-size))))
      (stdret 0 1)]
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
      (let ([id (LOWORD wParam)])
	(let ([item (id-to-menu-item id)])
	  (when item (send item auto-check)))
	(queue-window-event this (lambda () (on-menu-command id))))
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
     [(= msg WM_DISPLAYCHANGE)
      (parameterize ([current-eventspace (get-eventspace)])
        (queue-callback (lambda () (display-changed))))
      0]
     [else
      (super wndproc w msg wParam lParam default)]))

  (define/override (try-nc-mouse w msg wParam lParam)
    #f)

  (define/override (set-size x y w h)
    (unless (and (= w -1) (= h -1))
      (maximize #f))
    (super set-size x y w h))

  (define/public (on-close) #t)

  (define/override (is-shown-to-root?)
    (is-shown?))
  (define/override (is-enabled-to-root?)
    #t)

  (define/public (on-toolbar-click) (void))
  (define/public (on-menu-click) (void))

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
    (set! focus-window-path (cons this win-path))
    (when (ptr-equal? hwnd (GetActiveWindow))
      (void (SetFocus child-hwnd))))

  (define/private (set-frame-focus)
    (let ([p focus-window-path])
      (when (pair? p)
        (SetFocus (send (last p) get-focus-hwnd)))))

  (define/public (get-focus-window [even-if-not-active? #f])
    (let ([p focus-window-path])
      (and (pair? p)
           (or even-if-not-active?
               (ptr-equal? hwnd (GetActiveWindow)))
           (last p))))

  (define/override (can-accept-focus?)
    #f)
  (define/override (child-can-accept-focus?)
    #t)

  (define/public (on-activate on?) (void))

  (define/override (call-pre-on-event w e)
    (pre-on-event w e))
  (define/override (call-pre-on-char w e)
    (pre-on-char w e))

  (define modal-enabled? #t)
  (define otherwise-enabled? #t)
  (define/public (modal-enable ignoring)
    (define on? (not (other-modal? this #f ignoring)))
    (unless (eq? modal-enabled? on?)
      (set! modal-enabled? on?)
      (update-enabled)))
  (define/override (internal-enable on?)
    (set! otherwise-enabled? on?)
    (update-enabled))
  (define/private (update-enabled)
    (super internal-enable (and modal-enabled? otherwise-enabled?)))

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
  
  (define/override (refresh-all-children)
    (when saved-child
      (send saved-child refresh)))

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
          [y (box 0)]
	  [ww (box 0)]
	  [wh (box 0)]
	  [wx (box 0)]
	  [wy (box 0)])
      (display-size sw sh #f 0 void)
      (if wrt
	  (begin
	    (send wrt get-size ww wh)
	    (set-box! wx (send wrt get-x))
	    (set-box! wy (send wrt get-y)))
	  (begin
	    (set-box! ww (unbox sw))
	    (set-box! wh (unbox sh))))
      (get-size w h)
      (MoveWindow hwnd
                  (if (or (eq? mode 'both)
                          (eq? mode 'horizontal))
                      (max 0 
			   (min (- (unbox sw) (unbox w))
				(+ (quotient (- (unbox ww) (unbox w)) 2) 
				   (unbox wx))))
                      (get-x))
                  (if (or (eq? mode 'both)
                          (eq? mode 'vertical))
		      (max 0
			   (min (- (unbox sh) (unbox h))
				(+ (quotient (- (unbox wh) (unbox h)) 2) 
				   (unbox wy))))
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

  (define/public (designate-root-frame) (void))

  (define modified? #f)
  (define/public (set-modified on?)
    (unless (eq? modified? (and on? #t))
      (set! modified? (and on? #t))
      (set-title saved-title)))

  (define/public (is-maximized?)
    (if (is-shown?)
        (IsZoomed hwnd)
        hidden-zoomed?))
  
  (define/public (maximize on?)
    (if (is-shown?)
        (ShowWindow hwnd (if on?
                             SW_MAXIMIZE
                             SW_RESTORE))
        (set! hidden-zoomed? (and on? #t))))
    
  (define/public (iconized?)
    (IsIconic hwnd))

  (define/public (iconize on?)
    (when (is-shown?)
      (unless (eq? (and on? #t) (iconized?))
        (ShowWindow hwnd (if on? SW_MINIMIZE SW_RESTORE)))))

  (define/public (fullscreened?) #f)
  (define/public (fullscreen on?) (void))

  (define/private (get-placement)
    (let ([wp (make-WINDOWPLACEMENT 
               (ctype-sizeof _WINDOWPLACEMENT)
               0
               0
               (make-POINT 0 0)
               (make-POINT 0 0)
               (make-RECT 0 0 0 0))])
      (GetWindowPlacement hwnd wp)
      wp))

  (define/override (get-size w h)
    (if (iconized?)
        (let ([wp (get-placement)])
          (let ([r (WINDOWPLACEMENT-rcNormalPosition wp)])
            (set-box! w (- (RECT-right r) (RECT-left r)))
            (set-box! h (- (RECT-bottom r) (RECT-top r)))))
        (super get-size w h)))

  (define/override (get-client-size w h)
    (if (iconized?)
        (begin
          (get-size w h)
          (set-box! w (max 1 (- (unbox w) client-dw)))
          (set-box! h (max 1 (- (unbox h) client-dh))))
        (super get-client-size w h)))

  (define/override (get-x)
    (if (iconized?)
        (let ([wp (get-placement)])
          (RECT-left (WINDOWPLACEMENT-rcNormalPosition wp)))
        (RECT-left (GetWindowRect hwnd))))

  (define/override (get-y)
    (if (iconized?)
        (let ([wp (get-placement)])
          (RECT-top (WINDOWPLACEMENT-rcNormalPosition wp)))
        (RECT-top (GetWindowRect hwnd))))

  (define/override (get-width)
    (if (iconized?)
        (let ([w (box 0)])
          (get-size w (box 0))
          (unbox w))
        (super get-width)))

  (define/override (get-height)
    (if (iconized?)
        (let ([h (box 0)])
          (get-size (box 0) h)
          (unbox h))
        (super get-height)))

  (def/public-unimplemented get-menu-bar)

  (define menu-bar #f)
  (define/public (set-menu-bar mb)
    (atomically
     (set! menu-bar mb)
     (send mb set-parent this)))

  (define/public (draw-menu-bar)
    (DrawMenuBar hwnd))
  
  (define/override (is-frame?) #t)

  ;; Retain to aviod GC of the icon:
  (define small-hicon #f)
  (define big-hicon #f)

  (define/public (set-icon bm [mask #f] [mode 'both])
    (let* ([bg-hbitmap
            (let* ([bm (make-object bitmap% (send bm get-width) (send bm get-height))]
                   [dc (make-object bitmap-dc% bm)])
              (send dc set-brush "black" 'solid)
              (send dc draw-rectangle 0 0 (send bm get-width) (send bm get-height))
              (send dc set-bitmap #f)
              (bitmap->hbitmap bm #:b&w? #t))]
           [main-hbitmap (bitmap->hbitmap bm #:mask mask)]
           [hicon (CreateIconIndirect
                   (make-ICONINFO
                    #t 0 0
                    bg-hbitmap
                    main-hbitmap))])
      (DeleteObject bg-hbitmap)
      (DeleteObject main-hbitmap)
      (when (or (eq? mode 'small)
                (eq? mode 'both))
        (atomically
         (set! small-hicon hicon)
         (SendMessageW hwnd WM_SETICON 0 (cast hicon _HICON _LPARAM))))
      (when (or (eq? mode 'big)
                (eq? mode 'both))
        (atomically
         (set! big-hicon hicon)
         (SendMessageW hwnd WM_SETICON 1 (cast hicon _HICON _LPARAM))))))

  (define/public (set-title s)
    (atomically
     (set! saved-title s)
     (SetWindowTextW (get-hwnd) (string-append s (if modified? "*" "")))))

  (define/public (popup-menu-with-char c)
    (DefWindowProcW hwnd WM_SYSKEYDOWN (char->integer c) (arithmetic-shift 1 29))
    (DefWindowProcW hwnd WM_SYSCHAR (char->integer c) (arithmetic-shift 1 29)))

  (define/public (system-menu)
    (popup-menu-with-char #\space))

  (define/public (display-changed) (void)))

