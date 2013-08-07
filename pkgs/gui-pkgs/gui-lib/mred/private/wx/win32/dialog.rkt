#lang racket/base
(require racket/class
         (only-in racket/list last)
         ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "../common/freeze.rkt"
         "../common/dialog.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
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
    (inherit get-eventspace)

    (define/override (create-frame parent label x y w h style)
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
        (let ([x (or x 0)]
              [y (or y 0)])
          (MoveWindow hwnd x y w h #t))
        hwnd))

    (define/override (is-dialog?) #t)

    (define/override (direct-show on?)
      ;; atomic mode
      (when on? (super direct-show on?))
      (for ([f (in-list (get-top-level-windows (get-eventspace)))])
        (send f modal-enable (and (not on?) this)))
      (when (not on?) (super direct-show on?)))))
