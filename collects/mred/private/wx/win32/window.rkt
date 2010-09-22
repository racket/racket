#lang racket/base
(require ffi/unsafe
	 racket/class
	 racket/draw
          "../../syntax.rkt"
          "../common/freeze.rkt"
          "../common/queue.rkt"
	 "utils.rkt"
	 "types.rkt"
	 "const.rkt"
	 "wndclass.rkt"
	 "queue.rkt"
	 "theme.rkt"
         "key.rkt")

(provide window%
	 queue-window-event
	 queue-window-refresh-event
	 
	 CreateWindowExW
	 GetWindowRect)

(define (unhide-cursor) (void))

(define-user32 CreateWindowExW (_wfun _DWORD
				      _string/utf-16
				      _string/utf-16
				      _DWORD
				      _int _int _int _int
				      _HWND _HMENU _HINSTANCE _pointer
				      -> _HWND))
(define-user32 GetWindowRect (_wfun _HWND (rect : (_ptr o _RECT)) -> (r : _BOOL) ->
                                    (if r rect (failed 'GetWindowRect))))
(define-user32 GetClientRect (_wfun _HWND (rect : (_ptr o _RECT)) -> (r : _BOOL) ->
                                    (if r rect (failed 'GetClientRect))))

(define-gdi32 CreateFontIndirectW (_wfun _LOGFONT-pointer -> _HFONT))

(define-user32 SendMessageW (_wfun _HWND _UINT _WPARAM _LPARAM -> _LRESULT))

(define-user32 MoveWindow(_wfun _HWND _int _int _int _int _BOOL -> (r : _BOOL)
                                -> (unless r (failed 'MoveWindow))))

(define-user32 ShowWindow (_wfun _HWND _int -> (previously-shown? : _BOOL) -> (void)))

(define SW_SHOW 5)
(define SW_HIDE 0)

(define-user32 GetDialogBaseUnits (_fun -> _LONG))
(define measure-dc #f)

(define theme-hfont #f)

(define-values (dlu-x dlu-y)
  (let ([v (GetDialogBaseUnits)])
    (values (* 1/4 (bitwise-and v #xFFFF))
	    (* 1/8 (arithmetic-shift v -16)))))

(defclass window% object%
  (init-field parent hwnd)
  (init style)

  (super-new)
  
  (define eventspace (current-eventspace))

  (set-hwnd-wx! hwnd this)

  (define/public (get-hwnd) hwnd)
  (define/public (get-client-hwnd) hwnd)
  (define/public (get-eventspace) eventspace)
  
  (define/public (wndproc w msg wParam lParam)
    (cond
     [(= msg WM_SETFOCUS)
      (queue-window-event this (lambda () (on-set-focus)))
      0]
     [(= msg WM_KILLFOCUS)
      (queue-window-event this (lambda () (on-kill-focus)))
      0]
     [(= msg WM_SYSKEYDOWN)
      (when (or (= wParam VK_MENU) (= wParam VK_F4)) ;; F4 is close
        (unhide-cursor)
        (begin0
         (DefWindowProcW w msg wParam lParam)
         (do-key wParam lParam #f #f)))]
     [(= msg WM_KEYDOWN)
      (do-key wParam lParam #f #f)
      0]
     [(= msg WM_KEYUP)
      (do-key wParam lParam #f #t)
      0]
     [(= msg WM_SYSCHAR)
      (when (= wParam VK_MENU)
        (unhide-cursor)
        (begin0
         (DefWindowProcW w msg wParam lParam)
         (do-key wParam lParam #t #f)))]
     [(= msg WM_CHAR)
      (do-key wParam lParam #t #f)
      0]
     [else
      (DefWindowProcW w msg wParam lParam)]))

  (define/public (show on?)
    (direct-show on?))

  (define shown? #f)
  (define/public (direct-show on?)
    (set! shown? (and on? #t))
    (unless on? (not-focus-child this))
    (ShowWindow hwnd (if on? SW_SHOW SW_HIDE)))
  (unless (memq 'invisible style)
    (show #t))
  
  (def/public-unimplemented on-drop-file)

  (define/public (on-size w h) (void))

  (define/public (on-set-focus) (void))
  (define/public (on-kill-focus) (void))
  (define/public (get-handle) hwnd)

  (define/public (is-window-enabled?)
    #t)

  (define/public (is-enabled-to-root?)
    (and (is-window-enabled?)
	 (send parent is-enabled-to-root?)))

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
                      (if (= y -11111) (RECT-right r) y)
                      (if (= w -1) (- (RECT-right r) (RECT-left r)) w)
                      (if (= h -1) (- (RECT-bottom r) (RECT-top r)) h)
                      #t))
        (MoveWindow hwnd x y w h #t))
    (on-size w h)
    (unless (and (= w -1) (= h -1))
      (on-resized))
    (refresh))
  (define/public (move x y)
    (set-size x y -1 -1))

  (define/public (auto-size label min-w min-h dw dh)
    (unless theme-hfont
      (set! theme-hfont (CreateFontIndirectW (get-theme-logfont))))
    (SendMessageW hwnd WM_SETFONT (cast theme-hfont _HFONT _LPARAM) 0)
    (unless measure-dc
      (let* ([bm (make-object bitmap% 1 1)]
	     [dc (make-object bitmap-dc% bm)]
	     [font (make-object font% 8 'system)])
	(send dc set-font font)
	(set! measure-dc dc)))
    (let-values ([(w h d a) (send measure-dc get-text-extent label #f #t)]
		 [(->int) (lambda (v) (inexact->exact (floor v)))])
      (set-size -11111 -11111 
		(max (->int (+ w dw)) (->int (* dlu-x min-w)))
		(max (->int (+ h dh)) (->int (* dlu-y min-h))))))

  (def/public-unimplemented popup-menu)
  (def/public-unimplemented center)

  (define/public (get-parent) parent)

  (define/public (refresh) (void))
  (define/public (on-resized) (void))

  (def/public-unimplemented screen-to-client)
  (def/public-unimplemented client-to-screen)

  (define/public (drag-accept-files on?)
    (void))

  (def/public-unimplemented enable)
  (def/public-unimplemented get-position)

  (define/public (get-client-size w h)
    (let ([r (GetClientRect (get-client-hwnd))])
      (set-box! w (- (RECT-right r) (RECT-left r)))
      (set-box! h (- (RECT-bottom r) (RECT-top r)))))

  (define/public (get-size w h)
    (let ([r (GetWindowRect (get-client-hwnd))])
      (set-box! w (- (RECT-right r) (RECT-left r)))
      (set-box! h (- (RECT-bottom r) (RECT-top r)))))

  (def/public-unimplemented fit)
  (def/public-unimplemented set-cursor)

  (define/public (set-focus)
    (when (can-accept-focus?)
      (set-top-focus this null hwnd)))

  (define/public (can-accept-focus?)
    (child-can-accept-focus?))

  (define/public (child-can-accept-focus?)
    (and shown?
         (send parent child-can-accept-focus?)))

  (define/public (set-top-focus win win-path hwnd)
    (send parent set-top-focus win (cons this win-path) hwnd))
  (define/public (not-focus-child v)
    (send parent not-focus-child v))

  (def/public-unimplemented gets-focus?)
  (def/public-unimplemented centre)

  (define/private (do-key wParam lParam is-char? is-up?)
    (let ([e (make-key-event #f wParam lParam is-char? is-up? hwnd)])
      (and e
           (if (definitely-wants-event? e)
               (begin
                 (queue-window-event this (lambda () (dispatch-on-char/sync e)))
                 #t)
               (constrained-reply (get-eventspace)
                                  (lambda () (dispatch-on-char e #t))
                                  #t)))))

  (define/public (definitely-wants-event? e) 
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
    (void)))

;; ----------------------------------------

(define (queue-window-event win thunk)
  (queue-event (send win get-eventspace) thunk))

(define (queue-window-refresh-event win thunk)
  (queue-refresh-event (send win get-eventspace) thunk))
