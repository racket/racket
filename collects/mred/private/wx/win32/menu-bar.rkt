#lang scheme/base
(require scheme/class
         ffi/unsafe
         "../../lock.rkt"
         "../../syntax.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt")

(provide menu-bar%)

(define-user32 CreateMenu (_wfun -> _HMENU))
(define-user32 SetMenu (_wfun _HWND _HMENU -> (r : _BOOL)
                              -> (unless r (failed 'SetMenu))))
(define-user32 DrawMenuBar (_wfun _HWND -> (r : _BOOL)
                                  -> (unless r (failed 'DrawMenuBar))))

(define menu-bar% 
  (class object%
    (super-new)

    (define hmenu (CreateMenu))

    (define menus null)

    (def/public-unimplemented set-label-top)
    (def/public-unimplemented number)
    (def/public-unimplemented enable-top)
    (def/public-unimplemented delete)

    (public [append-item append])
    (define (append-item m lbl)
      (let ([l (append menus (list m))])
        (atomically
         (set! menus l)
         (send m set-parent this lbl hmenu))))

    (define/public (set-parent f)
      (SetMenu (send f get-hwnd) hmenu)
      (DrawMenuBar (send f get-hwnd)))))
