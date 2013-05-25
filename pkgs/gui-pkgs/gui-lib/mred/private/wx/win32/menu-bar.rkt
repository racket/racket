#lang racket/base
(require racket/class
         (only-in racket/list take drop)
         ffi/unsafe
         "../../lock.rkt"
         "../../syntax.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt")

(provide
 (protect-out menu-bar%))

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
    (define parent #f)

    (define/public (set-label-top pos str)
      (send (list-ref menus pos) set-menu-label hmenu pos str)
      (refresh))
      
    (define/public (number) (length menus))

    (define/public (enable-top pos on?)
      (send (list-ref menus pos) enable-self hmenu pos on?)
      (refresh))

    (define/public (delete which pos)
      (atomically
       (set! menus (append (take menus pos)
                           (drop menus (add1 pos))))
       (RemoveMenu hmenu pos MF_BYPOSITION)
       (refresh)))

    (define/private (refresh)
      (when parent
        (send parent draw-menu-bar)))

    (public [append-item append])
    (define (append-item m lbl)
      (let ([l (append menus (list m))])
        (atomically
         (set! menus l)
         (send m set-parent this lbl hmenu)))
      (refresh))

    (define/public (popup-menu-with-char c)
      (when parent
	(send parent popup-menu-with-char c)))

    (define/public (set-parent f)
      (SetMenu (send f get-hwnd) hmenu)
      (set! parent f)
      (send parent draw-menu-bar))))
