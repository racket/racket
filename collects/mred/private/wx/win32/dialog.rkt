#lang racket/base
(require racket/class
         (only-in racket/list last)
         ffi/unsafe
	 "../../syntax.rkt"
	 "../../lock.rkt"
	 "../common/queue.rkt"
	 "../common/freeze.rkt"
	 "../common/dialog.rkt"
         "utils.ss"
         "const.ss"
         "types.ss"
	 "window.rkt"
	 "frame.rkt"
         "wndclass.rkt")

(provide dialog%)

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

(define DS_MODALFRAME #x80)

(define dialog% 
  (class (dialog-mixin frame%)
    (super-new)

    (define/override (create-frame parent label w h style)
      (let ([hwnd
             (CreateDialogIndirectParamW hInstance
                                         (make-DLGTEMPLATE
                                          (bitwise-ior DS_MODALFRAME WS_CAPTION WS_SYSMENU WS_THICKFRAME)
                                          0 0
                                          0 0 w h
                                          0 0 0)
                                         (and parent (send parent get-hwnd))
                                         dialog-proc
                                         0)])
        (SetWindowTextW hwnd label)
        (MoveWindow hwnd 0 0 w h #t)
        hwnd))

    (define/override (is-dialog?) #t)))
