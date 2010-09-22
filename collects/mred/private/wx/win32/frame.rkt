#lang racket/base
(require racket/class
         (only-in racket/list last)
         ffi/unsafe
	 "../../syntax.rkt"
	 "../common/queue.rkt"
         "utils.ss"
         "const.ss"
         "types.ss"
	 "window.rkt"
         "wndclass.rkt")

(provide frame%)

(define-user32 SetLayeredWindowAttributes (_wfun _HWND _COLORREF _BYTE _DWORD -> _BOOL))
(define-user32 GetActiveWindow (_wfun -> _HWND))
(define-user32 SetFocus (_wfun _HWND -> _HWND))

(defclass frame% window%
  (init parent
	label
	x y w h
	style)
  
  (inherit get-hwnd 
	   is-shown?
	   get-eventspace
           on-size
           pre-on-char pre-on-event)

  (super-new [parent #f]
	     [hwnd
	      (CreateWindowExW 0 ; (bitwise-ior WS_EX_LAYERED)
			       "PLTFrame"
			       (if label label "")
			       WS_OVERLAPPEDWINDOW
			       0 0 w h
			       #f
			       #f
			       hInstance
			       #f)]
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
    (register-frame-shown this on?)
    (super direct-show on?))

  (define/private (stdret f d)
    (if (is-dialog?) d f))

  (define/override (wndproc w msg wParam lParam)
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
     [else (super wndproc w msg wParam lParam)]))

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
  (def/public-unimplemented on-menu-command)
  (def/public-unimplemented on-mdi-activate)

  (define/public (enforce-size min-x min-y max-x max-y step-x step-y)
    (void))

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
    (when focus-window-path
      (SetFocus (send (last focus-window-path) get-hwnd))))

  (define/override (child-can-accept-focus?)
    #t)

  (define/public (on-activate on?) (void))

  (define/override (call-pre-on-event w e)
    (pre-on-event w e))
  (define/override (call-pre-on-char w e)
    (pre-on-char w e))

  (define dialog-level 0)
  (define/public (frame-relative-dialog-status win) 
    (cond
     [(is-dialog?) (let ([dl (send win get-dialog-level)])
                     (cond
                      [(= dl dialog-level) 'same]
                      [(dl . > . dialog-level) #f]
                      [else 'other]))]
     [else #f]))


  (def/public-unimplemented designate-root-frame)
  (def/public-unimplemented system-menu)
  (def/public-unimplemented set-modified)
  (def/public-unimplemented is-maximized?)
  (def/public-unimplemented maximize)
  (def/public-unimplemented iconized?)
  (def/public-unimplemented get-menu-bar)

  (define/public (set-menu-bar mb) (void))

  (def/public-unimplemented set-icon)
  (def/public-unimplemented iconize)
  (def/public-unimplemented set-title))
