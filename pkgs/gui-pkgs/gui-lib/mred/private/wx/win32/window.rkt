#lang racket/base
(require ffi/unsafe
	 racket/class
	 racket/draw
	 racket/draw/unsafe/bstr
         "../../syntax.rkt"
         "../common/freeze.rkt"
         "../common/queue.rkt"
         "../common/event.rkt"
         "../common/local.rkt"
         "../../lock.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "wndclass.rkt"
         "queue.rkt"
         "theme.rkt"
         "cursor.rkt"
         "key.rkt"
	 "dc.rkt"
         "font.rkt")

(provide
 (protect-out window%
              queue-window-event
              queue-window-refresh-event
              location->window
              flush-display
	      get-default-control-font

              GetWindowRect
              GetClientRect

              _NMHDR))

(define (unhide-cursor) (void))

(define WM_PRINT                        #x0317)
(define WM_PRINTCLIENT                  #x0318)

(define MK_LBUTTON          #x0001)
(define MK_RBUTTON          #x0002)
(define MK_SHIFT            #x0004)
(define MK_CONTROL          #x0008)
(define MK_MBUTTON          #x0010)
(define MK_XBUTTON1         #x0020)
(define MK_XBUTTON2         #x0040)

(define HTHSCROLL           6)
(define HTVSCROLL           7)

(define-user32 GetWindowRect (_wfun _HWND (rect : (_ptr o _RECT)) -> (r : _BOOL) ->
                                    (if r rect (failed 'GetWindowRect))))
(define-user32 GetClientRect (_wfun _HWND (rect : (_ptr o _RECT)) -> (r : _BOOL) ->
                                    (if r rect (failed 'GetClientRect))))

(define-user32 ClientToScreen (_wfun _HWND _POINT-pointer -> (r : _BOOL)
                                     -> (unless r (failed 'ClientToScreen))))
(define-user32 ScreenToClient (_wfun _HWND _POINT-pointer -> (r : _BOOL)
                                     -> (unless r (failed 'ClientToScreen))))

(define-gdi32 CreateFontIndirectW (_wfun _LOGFONTW-pointer -> _HFONT))

(define-shell32 DragAcceptFiles (_wfun _HWND _BOOL -> _void))

(define _HDROP _pointer)
(define-shell32 DragQueryFileW (_wfun _HDROP _UINT _pointer _UINT -> _UINT))
(define-shell32 DragFinish (_wfun _HDROP -> _void))

(define-user32 SetCapture (_wfun _HWND -> _HWND))
(define-user32 ReleaseCapture (_wfun -> _BOOL))

(define-user32 WindowFromPoint (_wfun _POINT -> _HWND))
(define-user32 GetParent (_wfun _HWND -> _HWND))
(define-user32 SetParent (_wfun _HWND _HWND -> (r : _HWND)
                                -> (unless r (failed 'SetParent))))

(define-user32 SetCursorPos (_wfun _int _int -> _BOOL))

(define-cstruct _NMHDR
  ([hwndFrom _HWND]
   [idFrom _pointer]
   [code _UINT]))

(define-user32 GetDialogBaseUnits (_fun -> _LONG))
(define measure-dc #f)

(define theme-hfont #f)

#;
(define-values (dlu-x dlu-y)
  (let ([v (GetDialogBaseUnits)])
    (values (* 1/4 (bitwise-and v #xFFFF))
	    (* 1/8 (arithmetic-shift v -16)))))

(define-cstruct _LOGBRUSH
  ([lbStyle _UINT]
   [lbColor _COLORREF]
   [lbHatch _pointer]))

(define BS_NULL 1)
(define transparent-logbrush (make-LOGBRUSH BS_NULL 0 #f))

(define-gdi32 CreateBrushIndirect (_wfun _LOGBRUSH-pointer -> _HBRUSH))

(define TRANSPARENT 1)
(define-gdi32 SetBkMode (_wfun _HDC _int -> (r : _int)
                               -> (when (zero? r) (failed 'SetBkMode))))

(define-user32 BeginPaint (_wfun _HWND _pointer -> _HDC))
(define-user32 EndPaint (_wfun _HDC _pointer -> _BOOL))

(define WM_IS_GRACKET (cast (scheme_register_process_global "PLT_WM_IS_GRACKET" #f)
                            _pointer
                            _UINT_PTR))
(define GRACKET_GUID (cast (scheme_register_process_global "PLT_GRACKET_GUID" #f)
                           _pointer
                           _bytes))
(define-cstruct _COPYDATASTRUCT
  ([dwData _pointer]
   [cbData _DWORD]
   [lpData _pointer]))

(define-cstruct _TRACKMOUSEEVENT
  ([cbSize _DWORD]
   [dwFlags _DWORD]
   [hwndTrack _HWND]
   [dwHoverTime _DWORD]))

(define TME_LEAVE #x02)
(define TME_NONCLIENT #x10)

(define-user32 TrackMouseEvent (_wfun _TRACKMOUSEEVENT-pointer -> (r : _BOOL)
                                      -> (unless r (failed 'TrackMouseEvent))))
(define-user32 GetCursorPos (_wfun _POINT-pointer -> _BOOL))

(defclass window% object%
  (init-field parent hwnd)
  (init style
        [extra-hwnds null])

  (define enabled? #t)
  (define parent-enabled? #t)

  (super-new)
  
  (define eventspace (if parent
                         (send parent get-eventspace)
                         (current-eventspace)))

  (set-hwnd-wx! hwnd this)
  (for ([extra-hwnd (in-list extra-hwnds)])
    (set-hwnd-wx! extra-hwnd this))

  (define/public (get-hwnd) hwnd)
  (define/public (get-client-hwnd) hwnd)
  (define/public (get-content-hwnd) (get-client-hwnd))
  (define/public (get-focus-hwnd) hwnd)
  (define/public (get-eventspace) eventspace)

  (define/public (is-hwnd? a-hwnd)
    (ptr-equal? hwnd a-hwnd))
  
  (define/public (wndproc w msg wParam lParam default)
    (if (try-mouse w msg wParam lParam)
        0
        (cond
         [(= msg WM_SETFOCUS)
          (set-top-focus this null w)
          (queue-window-event this (lambda () (on-set-focus)))
          0]
         [(= msg WM_KILLFOCUS)
          (queue-window-event this (lambda () (on-kill-focus)))
          0]
         [(= msg WM_SYSKEYDOWN)
          (let ([result (if (or (= wParam VK_MENU) (= wParam VK_F4)) ;; F4 is close
			    (begin
			      (unhide-cursor)
			      (default w msg wParam lParam))
			    0)])
	    (do-key w msg wParam lParam #f #f void)
	    result)]
         [(= msg WM_KEYDOWN)
          (do-key w msg wParam lParam #f #f default)]
         [(= msg WM_KEYUP)
          (do-key w msg wParam lParam #f #t default)]
         [(= msg WM_SYSCHAR)
	  (let ([result (if (= wParam VK_MENU)
			    (begin
			      (unhide-cursor)
			      (default w msg wParam lParam))
			    0)])
	    (do-key w msg wParam lParam #t #f void)
	    result)]
         [(= msg WM_CHAR)
          (do-key w msg wParam lParam #t #f default)]
	 [(= msg WM_MOUSEWHEEL)
          (gen-wheels w msg lParam (HIWORD wParam) 'wheel-down 'wheel-up)
	  0]
         [(= msg WM_MOUSEHWHEEL) ; Vista and later
          (gen-wheels w msg lParam (HIWORD wParam) 'wheel-left 'wheel-right)
          0]
         [(= msg WM_COMMAND)
          (let* ([control-hwnd (cast lParam _LPARAM _HWND)]
                 [wx (any-hwnd->wx control-hwnd)]
                 [cmd (HIWORD wParam)])
            (if (and wx (send wx is-command? cmd))
                (begin
                  (send wx do-command cmd control-hwnd)
                  0)
                (default w msg wParam lParam)))]
         [(= msg WM_NOTIFY)
          (let* ([nmhdr (cast lParam _LPARAM _NMHDR-pointer)]
                 [control-hwnd (NMHDR-hwndFrom nmhdr)]
                 [wx (any-hwnd->wx control-hwnd)]
                 [cmd (LOWORD (NMHDR-code nmhdr))])
            (if (and wx (send wx is-command? cmd))
                (begin
                  (send wx do-command-ex cmd control-hwnd nmhdr)
                  0)
                (default w msg wParam lParam)))]
         [(or (= msg WM_HSCROLL)
              (= msg WM_VSCROLL))
          (let* ([control-hwnd (cast lParam _LPARAM _HWND)]
                 [wx (any-hwnd->wx control-hwnd)])
            (if wx
                (begin
                  (send wx control-scrolled)
                  0)
                (default w msg wParam lParam)))]
         [(= msg WM_DROPFILES)
          (handle-drop-files wParam)
          0]
         ;; for single-instance applications:
         [(and (= msg WM_IS_GRACKET)
               (positive? WM_IS_GRACKET))
          ;; return 79 to indicate that this is a GRacket window
          79]
         ;; also for single-instance:
         [(= msg WM_COPYDATA)
          (handle-copydata lParam)
          0]
	 [(= msg WM_INPUTLANGCHANGE)
	  (reset-key-mapping)
	  0]
         [else
          (default w msg wParam lParam)])))

  (define/public (is-command? cmd) #f)
  (define/public (control-scrolled) #f)

  (define/public (do-command cmd control-hwnd)
    (void))
  (define/public (do-command-ex cmd control-hwnd nmhdr)
    (do-command cmd control-hwnd))

  (define/public (show on?)
    (when on? (show-children))
    (atomically (direct-show on?)))

  (define shown? #f)
  (define/public (direct-show on? [on-mode SW_SHOW])
    ;; atomic mode
    (set! shown? (and on? #t))
    (register-child-in-parent on?)
    (unless on? (not-focus-child this))
    (ShowWindow hwnd (if on? on-mode SW_HIDE)))
  (unless (memq 'deleted style)
    (show #t))
  
  (define/public (queue-on-size) (void))

  (define/public (on-set-focus) (void))
  (define/public (on-kill-focus) (void))
  (define/public (get-handle) hwnd)
  (define/public (get-client-handle) (get-content-hwnd))

  (define/public (enable on?)
    (unless (eq? enabled? (and on? #t))
      (atomically
       (let ([prev? (and enabled? parent-enabled?)])
         (set! enabled? (and on? #t))
         (let ([now? (and parent-enabled? enabled?)])
           (unless (eq? now? prev?)
             (internal-enable now?)))))))
  (define/public (parent-enable on?)
    (unless (eq? on? parent-enabled?)
      (let ([prev? (and enabled? parent-enabled?)])
        (set! parent-enabled? (and on? #t))
        (let ([now? (and parent-enabled? enabled?)])
          (unless (eq? prev? now?)
            (internal-enable now?))))))
  (define/public (internal-enable on?)
    (void (EnableWindow hwnd on?)))

  (define/public (is-window-enabled?) enabled?)
  (define/public (is-enabled-to-root?)
    (and enabled? parent-enabled?))

  (define/public (is-shown-to-root?)
    (and shown?
	 (send parent is-shown-to-root?)))

  (define/public (is-shown?)
    shown?)

  (define/public (get-x)
    (let ([r (GetWindowRect hwnd)]
          [pr (GetWindowRect (send parent get-content-hwnd))])
      (- (RECT-left r) (RECT-left pr))))
  (define/public (get-y)
    (let ([r (GetWindowRect hwnd)]
          [pr (GetWindowRect (send parent get-content-hwnd))])
      (- (RECT-top r) (RECT-top pr))))

  (define/public (get-width)
    (let ([r (GetWindowRect hwnd)])
      (- (RECT-right r) (RECT-left r))))
  (define/public (get-height)
    (let ([r (GetWindowRect hwnd)])
      (- (RECT-bottom r) (RECT-top r))))

  (define/public (notify-child-extent x y)
    (void))

  (define/public (set-size x y w h)
    (let-values ([(x y w h)
                  (if (or (not x)
                          (not y)
                          (= w -1)
                          (= h -1))
                      (let ([r (GetWindowRect hwnd)])
                        (values (or x (RECT-left r))
                                (or y (RECT-top r))
                                (if (= w -1) (- (RECT-right r) (RECT-left r)) w)
                                (if (= h -1) (- (RECT-bottom r) (RECT-top r)) h)))
                      (values x y w h))])
      (when parent (send parent notify-child-extent (+ x w) (+ y h)))
      (MoveWindow hwnd x y w h #t))
    (unless (and (= w -1) (= h -1))
      (on-resized))
    (queue-on-size)
    (refresh-one))
  (define/public (move x y)
    (set-size x y -1 -1))

  (define/public (set-control-font font [hwnd hwnd])
    (unless theme-hfont
      (set! theme-hfont (CreateFontIndirectW (get-theme-logfont))))
    (let ([hfont (if font
                     (or (font->hfont font)
			 theme-hfont)
                     theme-hfont)])
      (SendMessageW hwnd WM_SETFONT (cast hfont _HFONT _LPARAM) 0)))

  (define/public (auto-size font label min-w min-h dw dh
                            [resize
                             (lambda (w h) (set-size #f #f w h))]
                            #:combine-width [combine-w max]
                            #:combine-height [combine-h max]
                            #:scale-w [scale-w 1]
                            #:scale-h [scale-h 1])
    (atomically
     (unless measure-dc
       (let* ([bm (make-object win32-bitmap% 1 1 #f)]
              [dc (make-object bitmap-dc% bm)])
         (set! measure-dc dc)))
     (send measure-dc set-font (or font
				   (get-default-control-font)))
     (let-values ([(w h d a) (let loop ([label label])
                               (cond
                                [(null? label) (values 0 0 0 0)]
                                [(label . is-a? . bitmap%)
                                 (values (send label get-width)
                                         (send label get-height)
                                         0
                                         0)]
                                [(pair? label)
                                 (let-values ([(w1 h1 d1 a1)
                                               (loop (car label))]
                                              [(w2 h2 d2 a2)
                                               (loop (cdr label))])
                                   (values (combine-w w1 w2) (combine-h h1 h2)
                                           (combine-h d1 d1) (combine-h a1 a2)))]
                                [else
                                 (define strs (regexp-split #rx"\n" label))
                                 (for/fold ([w 0][h 0][d 0] [a 0]) ([str (in-list strs)])
                                   (define-values (tw th d a) (send measure-dc get-text-extent label #f #t))
                                   (values (max w tw) (+ h th) 0 0))]))]
                  [(->int) (lambda (v) (inexact->exact (ceiling v)))])
       (resize (->int (* scale-h (max (+ w dw) min-w)))
               (->int (* scale-w (max (+ h dh) min-h)))))))

  (define/public (popup-menu m x y)
    (let ([gx (box x)]
          [gy (box y)])
      (client-to-screen gx gy)
      (send m popup (unbox gx) (unbox gy)
            hwnd
            (lambda (thunk) (queue-window-event this thunk)))))

  (define/public (center a b) (void))

  (define/public (get-parent) parent)
  (define/public (set-parent p) 
    ;; in atomic mode
    (set! parent p)
    (SetParent hwnd (send parent get-content-hwnd)))

  (define/public (is-frame?) #f)

  (define/public (refresh-one) (void))
  (define/public (refresh)
    (refresh-one)
    (refresh-all-children))
  (define/public (refresh-all-children) (void))

  (define/public (on-resized) (void))

  (define event-position-wrt-wx #f)
  (define/public (set-event-positions-wrt wx)
    (set! event-position-wrt-wx wx))

  (define/private (adjust-event-position x y)
    (if event-position-wrt-wx
        (let ([xb (box x)]
              [yb (box y)])
          (internal-client-to-screen xb yb)
          (send event-position-wrt-wx internal-screen-to-client xb yb)
          (values (unbox xb) (unbox yb)))
        (values x y)))

  (define/public (screen-to-client x y)
    (internal-screen-to-client x y))
  (define/public (internal-screen-to-client x y)
    (let ([p (make-POINT (unbox x) (unbox y))])
      (ScreenToClient (get-client-hwnd) p)
      (set-box! x (POINT-x p))
      (set-box! y (POINT-y p))))
  (define/public (client-to-screen x y)
    (internal-client-to-screen x y))
  (define/public (internal-client-to-screen x y)
    (let ([p (make-POINT (unbox x) (unbox y))])
      (ClientToScreen (get-client-hwnd) p)
      (set-box! x (POINT-x p))
      (set-box! y (POINT-y p))))

  (define/public (warp-pointer x y)
    (define xb (box x))
    (define yb (box y))
    (client-to-screen xb yb)
    (void (SetCursorPos (unbox xb) (unbox yb))))

  (define/public (in-content? p)
    (ScreenToClient (get-client-hwnd) p)
    (let ([r (GetClientRect (get-client-hwnd))])
      (and (< 0 (POINT-x p) (- (RECT-right r)
			       (RECT-left r)))
	   (< 0 (POINT-y p) (- (RECT-bottom r)
			       (RECT-top r))))))

  (define/public (drag-accept-files on?)
    (DragAcceptFiles (get-hwnd) on?))

  (define/private (handle-drop-files wParam)
    (let* ([hdrop (cast wParam _WPARAM _HDROP)]
           [count (DragQueryFileW hdrop #xFFFFFFFF #f 0)])
      (for ([i (in-range count)])
        (let* ([len (DragQueryFileW hdrop i #f 0)]
               [b (malloc (add1 len) _int16)])
          (DragQueryFileW hdrop i b (add1 len))
          (let ([s (cast b _gcpointer _string/utf-16)])
            (queue-window-event this (lambda () (on-drop-file (string->path s)))))))
      (DragFinish hdrop)))

  (define/public (on-drop-file p) (void))

  (define/public (get-client-size w h)
    (let ([r (GetClientRect (get-client-hwnd))])
      (set-box! w (- (RECT-right r) (RECT-left r)))
      (set-box! h (- (RECT-bottom r) (RECT-top r)))))

  (define/public (get-size w h)
    (let ([r (GetWindowRect (get-client-hwnd))])
      (set-box! w (- (RECT-right r) (RECT-left r)))
      (set-box! h (- (RECT-bottom r) (RECT-top r)))))

  (define cursor-handle #f)
  (define/public (set-cursor c)
    (set! cursor-handle (and c (send (send c get-driver) get-handle)))
    (when mouse-in?
      (cursor-updated-here)))

  (define/public (cursor-updated-here)
    (when mouse-in?
      (send (get-top-frame) reset-cursor (get-arrow-cursor))))

  (define/public (reset-cursor-in-child child default)
    (send child reset-cursor (or cursor-handle default)))

  (define effective-cursor-handle #f)
  (define/public (reset-cursor default)
    (let ([c (or cursor-handle default)])
      (set! effective-cursor-handle c)
      (SetCursor c)))

  (define/public (no-cursor-handle-here)
    (send parent cursor-updated-here))

  (define/public (set-focus [child-hwnd hwnd])
    (when (can-accept-focus?)
      (set-top-focus this null child-hwnd)))

  (define/public (can-accept-focus?)
    (child-can-accept-focus?))

  (define/public (child-can-accept-focus?)
    (and shown?
         (send parent child-can-accept-focus?)))

  (define/public (set-top-focus win win-path hwnd)
    (send parent set-top-focus win (cons this win-path) hwnd))
  (define/public (not-focus-child v)
    (send parent not-focus-child v))

  (define/public (gets-focus?) #f)

  (define/public (register-child child on?)
    (void))
  (define/public (register-child-in-parent on?)
    (when parent
      (send parent register-child this on?)))

  (define/public (show-children) (void))
  (define/public (paint-children) (void))

  (define/public (get-top-frame)
    (send parent get-top-frame))

  (define/private (gen-wheels w msg lParam val down up)
    (let ([orig-delta (quotient val WHEEL_DELTA)])
      (let loop ([delta (abs orig-delta)])
        (unless (zero? delta)
          (do-key w msg (if (negative? orig-delta)
                            down
                            up)
                  lParam #f #f void)
          (loop (sub1 delta))))))
  
  (define/private (do-key w msg wParam lParam is-char? is-up? default)
    (let ([e (maybe-make-key-event #f wParam lParam is-char? is-up? hwnd)])
      (if (and e
               (if (definitely-wants-event? w msg wParam e)
                   (begin
                     (queue-window-event this (lambda () (dispatch-on-char/sync e)))
                     #t)
                   (constrained-reply eventspace
                                      (lambda () (dispatch-on-char e #t))
                                      #t)))
          0
          (default w msg wParam lParam))))

  (define/public (try-mouse w msg wParam lParam)
    (cond
     [(= msg WM_RBUTTONDOWN)
      (do-mouse w msg #f 'right-down wParam lParam)]
     [(= msg WM_RBUTTONUP)
      (do-mouse w msg #f 'right-up wParam lParam)]
     [(= msg WM_RBUTTONDBLCLK)
      (do-mouse w msg #f 'right-down wParam lParam)]
     [(= msg WM_MBUTTONDOWN)
      (do-mouse w msg #f 'middle-down wParam lParam)]
     [(= msg WM_MBUTTONUP)
      (do-mouse w msg #f 'middle-up wParam lParam)]
     [(= msg WM_MBUTTONDBLCLK)
      (do-mouse w msg #f 'middle-down wParam lParam)]
     [(= msg WM_LBUTTONDOWN)
      (do-mouse w msg #f 'left-down wParam lParam)]
     [(= msg WM_LBUTTONUP)
      (do-mouse w msg #f 'left-up wParam lParam)]
     [(= msg WM_LBUTTONDBLCLK)
      (do-mouse w msg #f 'left-down wParam lParam)]
     [(= msg WM_MOUSEMOVE)
      (do-mouse w msg #f 'motion wParam lParam)]
     [(= msg WM_MOUSELEAVE)
      (let ([p (make-POINT 0 0)])
        (let ([f (and (GetCursorPos p)
		      (location->window (POINT-x p) (POINT-y p)))])
          (unless (and (eq? f (get-top-frame))
		       (send f in-content? p))
            (do-mouse w msg #f 'leave wParam lParam))))
      ;; send message on to default handling (e.g., for buttons):
      #f]
     [else (try-nc-mouse w msg wParam lParam)]))

  ;; Breaking out NC mouse operations lets us not handle
  ;; them for frames (where this method is overridden), 
  ;; since handling them intereferes with the cursor and
  ;; resize handling for frames.
  (define/public (try-nc-mouse w msg wParam lParam)
    (cond
     [(= msg WM_NCRBUTTONDOWN)
      (do-mouse w msg #t 'right-down wParam (lp-screen->client w lParam))]
     [(= msg WM_NCRBUTTONUP)
      (do-mouse w msg #t 'right-up wParam (lp-screen->client w lParam))]
     [(= msg WM_NCRBUTTONDBLCLK)
      (do-mouse w msg #t 'right-down wParam (lp-screen->client w lParam))]
     [(= msg WM_NCMBUTTONDOWN)
      (do-mouse w msg #t 'middle-down wParam (lp-screen->client w lParam))]
     [(= msg WM_NCMBUTTONUP)
      (do-mouse w msg #t 'middle-up wParam (lp-screen->client w lParam))]
     [(= msg WM_NCMBUTTONDBLCLK)
      (do-mouse w msg #t 'middle-down wParam (lp-screen->client w lParam))]
     [(= msg WM_NCLBUTTONDOWN)
      (do-mouse w msg #t 'left-down wParam (lp-screen->client w lParam))]
     [(= msg WM_NCLBUTTONUP)
      (do-mouse w msg #t 'left-up wParam (lp-screen->client w lParam))]
     [(= msg WM_NCLBUTTONDBLCLK)
      (do-mouse w msg #t 'left-down wParam (lp-screen->client w lParam))]
     [(and (= msg WM_NCMOUSEMOVE)
           (not (= wParam HTVSCROLL))
           (not (= wParam HTHSCROLL)))
      (do-mouse w msg #t 'motion wParam (lp-screen->client w lParam))]
     [else #f]))

  (define/private (lp-screen->client w lParam)
    (let ([p (make-POINT (LOWORD lParam) (HIWORD lParam))])
      (ScreenToClient w p)
      (MAKELPARAM (POINT-x p) (POINT-y p))))

  (define/private (do-mouse control-hwnd msg nc? type wParam lParam)
    (let ([x (LOWORD lParam)]
          [y (HIWORD lParam)]
          [flags (if nc? 0 wParam)]
          [bit? (lambda (v b) (not (zero? (bitwise-and v b))))])
      (let ([make-e 
             (lambda (type)
               (define-values (mx my) (adjust-event-position x y))
               (new mouse-event%
                    [event-type type]
                    [left-down (case type
                                 [(left-down) #t]
                                 [(left-up) #f]
                                 [else (bit? flags MK_LBUTTON)])]
                    [middle-down (case type
                                   [(middle-down) #t]
                                   [(middle-up) #f]
                                   [else (bit? flags MK_MBUTTON)])]
                    [right-down (case type
                                  [(right-down) #t]
                                  [(right-up) #f]
                                  [else (bit? flags MK_RBUTTON)])]
                    [x mx]
                    [y my]
                    [shift-down (bit? flags MK_SHIFT)]
                    [control-down (bit? flags MK_CONTROL)]
                    [meta-down #f]
                    [alt-down #f]
                    [time-stamp 0]
                    [caps-down #f]))])
        (if (eq? type 'leave)
            (let ([t (get-top-frame)])
              (send t send-child-leaves make-e)
              (send t send-leaves make-e))
            (begin
              (unless nc?
                (when (wants-mouse-capture? control-hwnd)
                  (when (memq type '(left-down right-down middle-down))
                    (SetCapture control-hwnd))
                  (when (memq type '(left-up right-up middle-up))
                    (ReleaseCapture))))
              (if mouse-in?
                  (if (send-child-leaves make-e)
                      (cursor-updated-here)
                      (if (send (get-top-frame) is-wait-cursor-on?)
                          (void (SetCursor (get-wait-cursor)))
                          (when effective-cursor-handle
                            (void (SetCursor effective-cursor-handle)))))
                  (let ([c (generate-mouse-ins this (lambda (type) (make-e type)))])
                    (TrackMouseEvent (make-TRACKMOUSEEVENT 
                                      (ctype-sizeof _TRACKMOUSEEVENT)
                                      (bitwise-ior TME_LEAVE)
                                      control-hwnd
                                      0))
                    (when c
                      (set! effective-cursor-handle c)
                      (void (SetCursor (if (send (get-top-frame) is-wait-cursor-on?)
                                           (get-wait-cursor)
                                           c))))))
              (when (memq type '(left-down right-down middle-down))
                (set-focus))
              (handle-mouse-event control-hwnd msg wParam (make-e type)))))))

  (define/private (handle-mouse-event w msg wParam e)
    (if (definitely-wants-event? w msg wParam e)
        (begin
          (queue-window-event this (lambda () (dispatch-on-event/sync e)))
          #t)
        (constrained-reply eventspace
                           (lambda () (dispatch-on-event e #t))
                           #t)))

  (define skip-enter-leave? #f)
  (define/public (skip-enter-leave-events skip?)
    (set! skip-enter-leave? skip?))

  (define mouse-in? #f)
  (define/public (generate-mouse-ins in-window mk)
    (if mouse-in?
        effective-cursor-handle
        (begin
          (set! mouse-in? #t)
          (let ([parent-cursor (generate-parent-mouse-ins mk)])
	    (unless skip-enter-leave?
              (handle-mouse-event (get-client-hwnd) 0 0 (mk 'enter)))
            (let ([c (or cursor-handle parent-cursor)])
              (set! effective-cursor-handle c)
              c)))))

  (define/public (generate-parent-mouse-ins mk)
    (send parent generate-mouse-ins this mk))

  (define/public (send-leaves mk)
    (when mouse-in?
      (set! mouse-in? #f)
      (unless skip-enter-leave?
        (when mk
          (let ([e (mk 'leave)])
	    (if (eq? (current-thread) 
		     (eventspace-handler-thread eventspace))
		(handle-mouse-event (get-client-hwnd) 0 0 e)
		(queue-window-event this
				    (lambda () (dispatch-on-event/sync e)))))))))

  (define/public (send-child-leaves mk)
    #f)

  (define/public (wants-mouse-capture? control-hwnd)
    #f)

  (define/public (definitely-wants-event? w msg wParam e)
    #f)

  (define/public (dispatch-on-char/sync e)
    (pre-event-refresh #t)
    (dispatch-on-char e #f))
  (define/public (dispatch-on-char e just-pre?) 
    (cond
     [(other-modal? this) #t]
     [(call-pre-on-char this e) #t]
     [just-pre? #f]
     [else (when (is-enabled-to-root?) (on-char e)) #t]))
  
  (define/public (dispatch-on-event/sync e)
    (pre-event-refresh #f)
    (dispatch-on-event e #f))
  (define/public (dispatch-on-event e just-pre?) 
    (cond
     [(other-modal? this e) #t]
     [(call-pre-on-event this e) #t]
     [just-pre? #f]
     [else (when (is-enabled-to-root?) (on-event e)) #t]))
  
  (define/public (call-pre-on-event w e)
    (or (send parent call-pre-on-event w e)
        (pre-on-event w e)))
  (define/public (call-pre-on-char w e)
    (or (send parent call-pre-on-char w e)
        (pre-on-char w e)))
  (define/public (pre-on-event w e) #f)
  (define/public (pre-on-char w e) #f)

  (define/public (on-char e) (void))
  (define/public (on-event e) (void))

  (define/private (pre-event-refresh key?)
    ;; Since we break the connection between the
    ;; Win32 queue and event handling, we
    ;; re-sync the display in case a stream of
    ;; events (e.g., key repeat) have a corresponding
    ;; stream of screen updates.
    (flush-display))

  (define/public (get-dialog-level) (send parent get-dialog-level)))

;; ----------------------------------------

(define (handle-copydata lParam)
  (let* ([cd (cast lParam _LPARAM _COPYDATASTRUCT-pointer)]
         [data (COPYDATASTRUCT-lpData cd)]
         [guid-len (bytes-length GRACKET_GUID)]
         [data-len (COPYDATASTRUCT-cbData cd)])
    (when (and (data-len
                . >= . 
                (+ guid-len 4 (ctype-sizeof _DWORD)))
               (bytes=? GRACKET_GUID
                        (scheme_make_sized_byte_string data
                                                       guid-len
                                                       0))
               (bytes=? #"OPEN"
                        (scheme_make_sized_byte_string (ptr-add data guid-len)
                                                       4
                                                       0)))
      ;; The command line's argv (sans argv[0]) is
      ;; expressed as a DWORD for the number of args,
      ;; followed by each arg. Each arg is a DWORD
      ;; for the number of chars and then the chars
      (let ([args
             (let ([count (ptr-ref data _DWORD 'abs (+ guid-len 4))])
               (let loop ([i 0] [delta (+ guid-len 4 (ctype-sizeof _DWORD))])
                 (if (or (= i count)
                         ((+ delta (ctype-sizeof _DWORD)) . > . data-len))
                     null
                     (let ([len (ptr-ref (ptr-add data delta) _DWORD)]
                           [delta (+ delta (ctype-sizeof _DWORD))])
                       (if ((+ delta len) . > . data-len)
                           null
                           (let ([s (scheme_make_sized_byte_string 
                                     (ptr-add data delta)
                                     len
                                     1)])
                             (if (or (bytes=? s #"")
                                     (regexp-match? #rx"\0" s))
                                 null
                                 (cons (bytes->path s)
                                       (loop (add1 i) (+ delta len))))))))))])
        (map queue-file-event args)))))

;; ----------------------------------------

(define default-control-font #f)
(define (get-default-control-font)
  (unless default-control-font
    (set! default-control-font
	  (make-object font%
		       (get-theme-font-size)
                       (logfont->pango-family
                         (get-theme-logfont))
		       'system
		       'normal 'normal #f 'default
		       #t)))
  default-control-font)

(define (queue-window-event win thunk)
  (queue-event (send win get-eventspace) thunk))

(define (queue-window-refresh-event win thunk)
  (queue-refresh-event (send win get-eventspace) thunk))

(define (location->window x y)
  (let ([hwnd (WindowFromPoint (make-POINT x y))])
    (let loop ([hwnd hwnd])
      (and hwnd
           (or (let ([wx (any-hwnd->wx hwnd)])
                 (and wx (send wx get-top-frame)))
               (loop (GetParent hwnd)))))))

(define (flush-display)
  (atomically
   (pre-event-sync #t)))
