#lang racket/base
(require ffi/unsafe
	 racket/class
	 racket/draw
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
         "key.rkt")

(provide window%
	 queue-window-event
	 queue-window-refresh-event
	 
	 GetWindowRect
         GetClientRect)

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

(define-gdi32 CreateFontIndirectW (_wfun _LOGFONT-pointer -> _HFONT))
(define-user32 FillRect (_wfun _HDC _RECT-pointer _HBRUSH -> (r : _int)
                               -> (when (zero? r) (failed 'FillRect))))

(define-user32 SetCapture (_wfun _HWND -> _HWND))
(define-user32 ReleaseCapture (_wfun -> _BOOL))

(define-cstruct _NMHDR
  ([hwndFrom _HWND]
   [idFrom _pointer]
   [code _UINT]))

(define-user32 GetDialogBaseUnits (_fun -> _LONG))
(define measure-dc #f)

(define theme-hfont #f)

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

(defclass window% object%
  (init-field parent hwnd)
  (init style
        [extra-hwnds null])

  (super-new)
  
  (define eventspace (current-eventspace))

  (set-hwnd-wx! hwnd this)
  (for ([extra-hwnd (in-list extra-hwnds)])
    (set-hwnd-wx! extra-hwnd this))

  (define/public (get-hwnd) hwnd)
  (define/public (get-client-hwnd) hwnd)
  (define/public (get-focus-hwnd) hwnd)
  (define/public (get-eventspace) eventspace)

  (define/public (is-hwnd? a-hwnd)
    (ptr-equal? hwnd a-hwnd))
  
  (define/public (wndproc w msg wParam lParam default)
    (if (try-mouse w msg wParam lParam)
        0
        (cond
         [(= msg WM_DESTROY)
          ;; release immobile cell 
          (unregister-hwnd w)
          ;; so it won't be finalized:
          (NotifyWindowDestroy w)]
         [(= msg WM_SETFOCUS)
          (queue-window-event this (lambda () (on-set-focus)))
          0]
         [(= msg WM_KILLFOCUS)
          (queue-window-event this (lambda () (on-kill-focus)))
          0]
         [(and (= msg WM_SYSKEYDOWN)
               (or (= wParam VK_MENU) (= wParam VK_F4))) ;; F4 is close
          (unhide-cursor)
          (begin0
           (default w msg wParam lParam)
           (do-key w msg wParam lParam #f #f))]
         [(= msg WM_KEYDOWN)
          (do-key w msg wParam lParam #f #f default)]
         [(= msg WM_KEYUP)
          (do-key w msg wParam lParam #f #t default)]
         [(and (= msg WM_SYSCHAR)
               (= wParam VK_MENU))
          (unhide-cursor)
          (begin0
           (default w msg wParam lParam)
           (do-key w msg wParam lParam #t #f void))]
         [(= msg WM_CHAR)
          (do-key w msg wParam lParam #t #f default)]
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
                  (send wx do-command control-hwnd)
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
         [else
          (default w msg wParam lParam)])))

  (define/public (is-command? cmd) #f)
  (define/public (control-scrolled) #f)

  (define/public (show on?)
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
  
  (def/public-unimplemented on-drop-file)

  (define/public (on-size w h) (void))

  (define/public (on-set-focus) (void))
  (define/public (on-kill-focus) (void))
  (define/public (get-handle) hwnd)

  (define enabled? #t)
  (define parent-enabled? #t)
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

  (def/public-unimplemented set-phantom-size)

  (define/public (paint-children) (void))

  (define/public (get-x)
    (let ([r (GetWindowRect hwnd)])
      (- (RECT-left r) (send parent get-x))))
  (define/public (get-y)
    (let ([r (GetWindowRect hwnd)])
      (- (RECT-top r) (send parent get-y))))

  (define/public (get-width)
    (let ([r (GetWindowRect hwnd)])
      (- (RECT-right r) (RECT-left r))))
  (define/public (get-height)
    (let ([r (GetWindowRect hwnd)])
      (- (RECT-bottom r) (RECT-top r))))

  (define/public (set-size x y w h)
    (if (or (= x -11111)
            (= y -11111)
            (= w -1)
            (= h -1))
        (let ([r (GetWindowRect hwnd)])
          (MoveWindow hwnd 
                      (if (= x -11111) (RECT-left r) x)
                      (if (= y -11111) (RECT-top r) y)
                      (if (= w -1) (- (RECT-right r) (RECT-left r)) w)
                      (if (= h -1) (- (RECT-bottom r) (RECT-top r)) h)
                      #t))
        (MoveWindow hwnd x y w h #t))
    (unless (and (= w -1) (= h -1))
      (on-resized))
    (refresh))
  (define/public (move x y)
    (set-size x y -1 -1))

  (define/public (set-control-font font [hwnd hwnd])
    (unless theme-hfont
      (set! theme-hfont (CreateFontIndirectW (get-theme-logfont))))
    (SendMessageW hwnd WM_SETFONT (cast theme-hfont _HFONT _LPARAM) 0))

  (define/public (auto-size label min-w min-h dw dh
                            [resize
                             (lambda (w h) (set-size -11111 -11111 w h))]
                            #:combine-width [combine-w max]
                            #:combine-height [combine-h max])
    (unless measure-dc
      (let* ([bm (make-object bitmap% 1 1)]
	     [dc (make-object bitmap-dc% bm)]
	     [font (make-object font% 8 'system)])
	(send dc set-font font)
	(set! measure-dc dc)))
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
                                (send measure-dc get-text-extent label #f #t)]))]
		 [(->int) (lambda (v) (inexact->exact (floor v)))])
      (resize (max (->int (+ w dw)) (->int (* dlu-x min-w)))
              (max (->int (+ h dh)) (->int (* dlu-y min-h))))))

  (define/public (popup-menu m x y)
    (let ([gx (box x)]
          [gy (box y)])
      (client-to-screen gx gy)
      (send m popup (unbox gx) (unbox gy)
            hwnd
            (lambda (thunk) (queue-window-event this thunk)))))

  (def/public-unimplemented center)

  (define/public (get-parent) parent)
  (define/public (is-frame?) #f)

  (define/public (refresh) (void))
  (define/public (on-resized) (void))

  (define/public (screen-to-client x y)
    (let ([p (make-POINT (unbox x) (unbox y))])
      (ScreenToClient (get-client-hwnd) p)
      (set-box! x (POINT-x p))
      (set-box! y (POINT-y p))))
  (define/public (client-to-screen x y)
    (let ([p (make-POINT (unbox x) (unbox y))])
      (ClientToScreen (get-client-hwnd) p)
      (set-box! x (POINT-x p))
      (set-box! y (POINT-y p))))

  (define/public (drag-accept-files on?)
    (void))

  (define/public (get-position x y)
    (set-box! x (get-x))
    (set-box! y (get-y)))

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
  (def/public-unimplemented centre)

  (define/public (register-child child on?)
    (void))
  (define/public (register-child-in-parent on?)
    (when parent
      (send parent register-child this on?)))

  (define/public (get-top-frame)
    (send parent get-top-frame))
  
  (define/private (do-key w msg wParam lParam is-char? is-up? default)
    (let ([e (make-key-event #f wParam lParam is-char? is-up? hwnd)])
      (if (and e
               (if (definitely-wants-event? w msg wParam e)
                   (begin
                     (queue-window-event this (lambda () (dispatch-on-char/sync e)))
                     #t)
                   (constrained-reply (get-eventspace)
                                      (lambda () (dispatch-on-char e #t))
                                      #t)))
          0
          (default w msg wParam lParam))))

  (define/public (try-mouse w msg wParam lParam)
    (cond
     [(= msg WM_NCRBUTTONDOWN)
      (do-mouse w msg #t 'right-down wParam lParam)]
     [(= msg WM_NCRBUTTONUP)
      (do-mouse w msg #t 'right-up wParam lParam)]
     [(= msg WM_NCRBUTTONDBLCLK)
      (do-mouse w msg #t 'right-down wParam lParam)]
     [(= msg WM_NCMBUTTONDOWN)
      (do-mouse w msg #t 'middle-down wParam lParam)]
     [(= msg WM_NCMBUTTONUP)
      (do-mouse w msg #t 'middle-up wParam lParam)]
     [(= msg WM_NCMBUTTONDBLCLK)
      (do-mouse w msg #t 'middle-down wParam lParam)]
     [(= msg WM_NCLBUTTONDOWN)
      (do-mouse w msg #t 'left-down wParam lParam)]
     [(= msg WM_NCLBUTTONUP)
      (do-mouse w msg #t 'left-up wParam lParam)]
     [(= msg WM_NCLBUTTONDBLCLK)
      (do-mouse w msg #t 'left-down wParam lParam)]
     [(and (= msg WM_NCMOUSEMOVE)
           (not (= wParam HTVSCROLL))
           (not (= wParam HTHSCROLL)))
      (do-mouse w msg #t 'motion wParam lParam)]
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
      (do-mouse w msg #f 'leave wParam lParam)]
     [else #f]))

  (define/private (do-mouse control-hwnd msg nc? type wParam lParam)
    (let ([x (LOWORD lParam)]
          [y (HIWORD lParam)]
          [flags (if nc? 0 wParam)]
          [bit? (lambda (v b) (not (zero? (bitwise-and v b))))])
      (let ([make-e 
             (lambda (type)
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
                    [x x]
                    [y y]
                    [shift-down (bit? flags MK_SHIFT)]
                    [control-down (bit? flags MK_CONTROL)]
                    [meta-down #f]
                    [alt-down #f]
                    [time-stamp 0]
                    [caps-down #f]))])
        (unless nc?
          (when (wants-mouse-capture? control-hwnd)
            (when (memq type '(left-down right-down middle-down))
              (SetCapture control-hwnd))
            (when (memq type '(left-up right-up middle-up))
              (ReleaseCapture))))
        (if mouse-in?
            (if (send-child-leaves (lambda (type) (make-e type)))
                (cursor-updated-here)
                (if (send (get-top-frame) is-wait-cursor-on?)
                    (void (SetCursor (get-wait-cursor)))
                    (when effective-cursor-handle
                      (void (SetCursor effective-cursor-handle)))))
            (let ([c (generate-mouse-ins this (lambda (type) (make-e type)))])
              (when c
                (set! effective-cursor-handle c)
                (void (SetCursor (if (send (get-top-frame) is-wait-cursor-on?)
                                     (get-wait-cursor)
                                     c))))))
        (when (memq type '(left-down right-down middle-down))
          (set-focus))
        (handle-mouse-event control-hwnd msg wParam (make-e type)))))

  (define/private (handle-mouse-event w msg wParam e)
    (if (definitely-wants-event? w msg wParam e)
        (begin
          (queue-window-event this (lambda () (dispatch-on-event/sync e)))
          #t)
        (constrained-reply (get-eventspace)
                           (lambda () (dispatch-on-event e #t))
                           #t)))

  (define mouse-in? #f)
  (define/public (generate-mouse-ins in-window mk)
    (if mouse-in?
        effective-cursor-handle
        (begin
          (set! mouse-in? #t)
          (let ([parent-cursor (generate-parent-mouse-ins mk)])
            (handle-mouse-event (get-client-hwnd) 0 0 (mk 'enter))
            (let ([c (or cursor-handle parent-cursor)])
              (set! effective-cursor-handle c)
              c)))))

  (define/public (generate-parent-mouse-ins mk)
    (send parent generate-mouse-ins this mk))

  (define/public (send-leaves mk)
    (set! mouse-in? #f)
    (let ([e (mk 'leave)])
      (if (eq? (current-eventspace) (get-eventspace))
          (handle-mouse-event (get-client-hwnd) 0 0 e)
          (queue-window-event this
                              (lambda () (dispatch-on-event/sync e))))))

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
     [(other-modal? this) #t]
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
    ;; Cocoa queue and event handling, we
    ;; re-sync the display in case a stream of
    ;; events (e.g., key repeat) have a corresponding
    ;; stream of screen updates.
    (void))

  (define/public (get-dialog-level) (send parent get-dialog-level)))

;; ----------------------------------------

(define (queue-window-event win thunk)
  (queue-event (send win get-eventspace) thunk))

(define (queue-window-refresh-event win thunk)
  (queue-refresh-event (send win get-eventspace) thunk))
