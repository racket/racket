#lang racket/base
(require racket/class
         (only-in racket/list last)
         ffi/unsafe
	 "../../syntax.rkt"
	 "../../lock.rkt"
	 "../common/queue.rkt"
	 "../common/freeze.rkt"
         "utils.ss"
         "const.ss"
         "types.ss"
	 "window.rkt"
	 "frame.rkt"
         "wndclass.rkt")

(provide dialog%)

(define _WORD _short)

(define-cstruct _DLGTEMPLATE
  ([style _DWORD]
   [dwExtendedStyle _DWORD]
   [cdit _WORD]
   [x _short]
   [y _short]
   [cx _short]
   [cy _short]
   [menu _short] ; 0
   [class _short] ; 0
   [title _short])) ; 0

(define _INT_PTR _long)
(define _DialogProc (_wfun _HWND _UINT _WPARAM _LPARAM -> _INT_PTR))


(define DS_MODALFRAME #x80)

(define-user32 CreateDialogIndirectParamW (_wfun _HINSTANCE
                                                 _DLGTEMPLATE-pointer
                                                 _HWND
                                                 _fpointer
                                                 -> _HWND))

(define (dlgproc w msg wParam lParam)
  (let ([wx (hwnd->wx w)])
    (if wx
        (send wx wndproc w msg wParam lParam
              (lambda (w msg wParam lParam) 0))
        0)))

(define dialog-proc (function-ptr dlgproc _DialogProc))

(define dialog-level-counter 0)

(define dialog% 
  (class frame%
    (super-new)

    (define/override (create-frame parent label w h)
      (let ([hwnd
             (CreateDialogIndirectParamW hInstance
                                         (make-DLGTEMPLATE
                                          (bitwise-ior DS_MODALFRAME WS_CAPTION WS_SYSMENU WS_THICKFRAME)
                                          0 0
                                          0 0 w h
                                          0 0 0)
                                         (and parent (send parent get-hwnd))
                                         dialog-proc)])
        (SetWindowTextW hwnd label)
        (MoveWindow hwnd 0 0 w h #t)
        hwnd))

    (define/override (is-dialog?) #t)

    (define dialog-level 0)
    (define/override (get-dialog-level) dialog-level)
    
    (define/override (frame-relative-dialog-status win) 
      (let ([dl (send win get-dialog-level)])
        (cond
         [(= dl dialog-level) 'same]
         [(dl . > . dialog-level) #f]
         [else 'other])))

    (define/override (direct-show on?)
      (when on?
        (set! dialog-level-counter (add1 dialog-level-counter))
        (set! dialog-level dialog-level-counter))
      (unless on?
        (set! dialog-level 0))
      (super direct-show on?))))

