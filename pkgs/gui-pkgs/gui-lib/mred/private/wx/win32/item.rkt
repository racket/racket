#lang racket/base
(require racket/class
         racket/draw
         ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/event.rkt"
	 "utils.rkt"
	 "const.rkt"
	 "window.rkt"
	 "wndclass.rkt"
         "hbitmap.rkt"
         "types.rkt")

(provide
 (protect-out item-mixin
              item%))

(define (item-mixin %)
  (class %
    (inherit on-set-focus
             on-kill-focus
             wndproc)

    (init-field [callback void])
    (define/public (command e)
      (callback this e))

    (super-new)
    
    (define/public (ctlproc w msg wParam lParam default)
     (cond
      [(= msg WM_SETFOCUS)
       (queue-window-event this (lambda () (on-set-focus)))
       (default w msg wParam lParam)]
      [(= msg WM_KILLFOCUS)
       (queue-window-event this (lambda () (on-kill-focus)))
       (default w msg wParam lParam)]
      [else
       (wndproc-for-ctlproc w msg wParam lParam default)]))

    (define/public (wndproc-for-ctlproc w msg wParam lParam default)
      (wndproc w msg wParam lParam default))))

(define item% 
  (class (item-mixin window%)
    (inherit get-hwnd)
    
    (super-new)
    
    (define/override (gets-focus?) #t)

    ;; Retain to avoid GC of the bitmaps:
    (define label-hbitmap #f)
    (define/public (remember-label-bitmap hbitmap)
      (set! label-hbitmap hbitmap))
    
    (define/public (set-label s)
      (if (s . is-a? . bitmap%)
          (let ([hbitmap (bitmap->hbitmap s)])
            (atomically
             (set! label-hbitmap hbitmap)
             (SendMessageW (get-hwnd) 
                           (get-setimage-message)
                           IMAGE_BITMAP 
                           (cast hbitmap _HBITMAP _LPARAM))))
          (SetWindowTextW (get-hwnd) s)))

    (define/public (get-setimage-message) BM_SETIMAGE)
    
    (def/public-unimplemented get-label)))


