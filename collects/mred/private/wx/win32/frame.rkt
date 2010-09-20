#lang racket/base
(require racket/class
	 "../../syntax.rkt"
	 "../common/queue.rkt"
         "utils.ss"
         "const.ss"
         "types.ss"
	 "window.rkt"
         "wndclass.rkt")

(provide frame%)

(define-user32 SetLayeredWindowAttributes (_wfun _HWND _COLORREF _BYTE _DWORD -> _BOOL))

(defclass frame% window%
  (init parent
	label
	x y w h
	style)
  
  (inherit get-win32 
	   is-shown?
	   get-eventspace)

  (super-new [parent #f]
	     [win32
	      (CreateWindowExW (bitwise-ior WS_EX_LAYERED)
			       "PLTFrame"
			       (if label label "")
			       WS_OVERLAPPEDWINDOW
			       0 0 w h
			       #f
			       #f
			       hInstance
			       #f)]
	     [style (cons 'invisible style)])

  (define win32 (get-win32))
  (SetLayeredWindowAttributes win32 0 255 LWA_ALPHA)

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

  (define/override (wndproc w msg wparam lparam)
    (cond
     [(= msg WM_CLOSE)
      (queue-window-event this (lambda () 
				 (when (on-close)
				   (direct-show #f))))
      0]
     [else (super wndproc w msg wparam lparam)]))

  (define/public (on-close) (void))

  (define/override (is-shown-to-root?)
    (is-shown?))
  (define/override (is-enabled-to-root?)
    #t)

  (define/override (get-x)
    (RECT-left (GetWindowRect win32)))
  (define/override (get-y)
    (RECT-top (GetWindowRect win32)))

  (def/public-unimplemented on-toolbar-click)
  (def/public-unimplemented on-menu-click)
  (def/public-unimplemented on-menu-command)
  (def/public-unimplemented on-mdi-activate)

  (define/public (enforce-size min-x min-y max-x max-y step-x step-y)
    (void))

  (def/public-unimplemented on-activate)
  (def/public-unimplemented designate-root-frame)
  (def/public-unimplemented system-menu)
  (def/public-unimplemented set-modified)
  (def/public-unimplemented is-maximized?)
  (def/public-unimplemented maximize)
  (def/public-unimplemented iconized?)
  (def/public-unimplemented get-menu-bar)
  (def/public-unimplemented set-menu-bar)
  (def/public-unimplemented set-icon)
  (def/public-unimplemented iconize)
  (def/public-unimplemented set-title))
