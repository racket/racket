#lang racket/base
(require racket/class
         racket/draw
         ffi/unsafe
          "../../syntax.rkt"
          "../common/event.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt"
         "hbitmap.rkt"
         "types.rkt")

(provide item-mixin
         item%)

(define (control-proc w msg wParam lParam)
  (let ([wx (hwnd->wx w)])
    (if wx
        (send wx ctlproc w msg wParam lParam
              (lambda (w msg wParam lParam)
                (send wx default-ctlproc w msg wParam lParam)))
        (send wx default-ctlproc w msg wParam lParam))))

(define control_proc (function-ptr control-proc _WndProc))

(define (item-mixin %)
  (class %
    (inherit on-set-focus
             on-kill-focus
             try-mouse
             wndproc)

    (init-field [callback void])
    (define/public (command e)
      (callback this e))

    (define old-control-procs null)

    (super-new)
    
    (define/public (subclass-control hwnd)
      (let ([old-control-proc (function-ptr (GetWindowLongW hwnd GWLP_WNDPROC) _WndProc)])
        (set! old-control-procs (cons (cons hwnd old-control-proc)
                                      old-control-procs))
        (SetWindowLongW hwnd GWLP_WNDPROC control_proc)))
    
    (define/public (ctlproc w msg wParam lParam default)
      (if (try-mouse w msg wParam lParam)
          0
          (cond
           [(= msg WM_SETFOCUS)
            (queue-window-event this (lambda () (on-set-focus)))
            (default w msg wParam lParam)]
           [(= msg WM_KILLFOCUS)
            (queue-window-event this (lambda () (on-kill-focus)))
            (default w msg wParam lParam)]
           [else
            (wndproc-for-ctlproc w msg wParam lParam default)])))

    (define/public (wndproc-for-ctlproc w msg wParam lParam default)
      (wndproc w msg wParam lParam default))
    
    (define/public (default-ctlproc w msg wParam lParam)
      (let loop ([l old-control-procs])
        (cond
         [(null? l) (error 'default-ctlproc "cannot find control in: ~e for: ~e" this w)]
         [(ptr-equal? (caar l) w)
          ((cdar l) w msg wParam lParam)]
         [else (loop (cdr l))])))))

(define item% 
  (class (item-mixin window%)
    (inherit get-hwnd)
    
    (super-new)
    
    (define/override (gets-focus?) #t)
    
    (define/public (set-label s)
      (SetWindowTextW (get-hwnd) s))
    
    (def/public-unimplemented get-label)))


