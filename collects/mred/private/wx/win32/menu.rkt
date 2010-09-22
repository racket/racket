#lang scheme/base
(require scheme/class
         ffi/unsafe
         "../../lock.rkt"
         "../../syntax.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "menu-item.rkt")

(provide menu%)

(define-user32 CreatePopupMenu (_wfun -> _HMENU))
(define-user32 AppendMenuW (_wfun _HMENU _UINT _pointer _string/utf-16 -> (r : _BOOL)
                                  -> (unless r (failed 'AppendMenuW))))

(defclass menu% object%
  (init lbl
        callback
        font)

  (define label lbl)
  (define parent #f)
  (define items null)

  (define hmenu (CreatePopupMenu))

  (define/public (set-parent p lbl parent-hmenu)
    (set! label lbl)
    (set! parent p)
    (AppendMenuW parent-hmenu 
                 (bitwise-ior MF_POPUP MF_STRING)
                 hmenu
                 lbl))

  (def/public-unimplemented select)
  (def/public-unimplemented get-font)
  (def/public-unimplemented set-width)
  (def/public-unimplemented set-title)
  (def/public-unimplemented set-label)
  (def/public-unimplemented set-help-string)
  (def/public-unimplemented number)
  (def/public-unimplemented enable)
  (def/public-unimplemented check)
  (def/public-unimplemented checked?)
  (def/public-unimplemented delete-by-position)
  (def/public-unimplemented delete)

  (public [append-item append])
  (define (append-item i label help-str-or-submenu chckable?)
    (let ([id (send (id-to-menu-item i) set-parent this label chckable?)])
      (atomically
       (set! items (append items (list i)))
       (AppendMenuW hmenu (bitwise-ior MF_STRING) (cast id _long _pointer) label))))

  (define/public (append-separator) 
    (atomically
     (set! items (append items (list #f)))
     (AppendMenuW hmenu MF_SEPARATOR #f #f)))

  (super-new))
