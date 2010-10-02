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
(define-user32 EnableMenuItem (_wfun _HMENU _UINT _UINT -> _BOOL))

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

  (define/private (with-item id proc)
    (let loop ([items items] [pos 0])
      (cond
       [(null? items) (void)]
       [(and (car items)
             (eq? id (send (car items) id)))
        (proc (car items) pos)]
       [else (loop (cdr items) (add1 pos))])))

  (define/public (set-menu-label bar-hmenu pos str)
    (ModifyMenuW bar-hmenu pos
                 (bitwise-ior MF_BYPOSITION MF_STRING MF_POPUP)
                 (cast hmenu _HMENU _UINT_PTR)
                 str))

  (define/public (set-label id str)
    (with-item
     id
     (lambda (i pos)
       (send i set-label hmenu pos str))))

  (def/public-unimplemented set-help-string)
  (def/public-unimplemented number)

  (define/public (enable id on?)
    (with-item
     id
     (lambda (i pos)
       (void
        (EnableMenuItem hmenu pos 
                        (bitwise-ior MF_BYPOSITION
                                     (if on? MF_ENABLED MF_GRAYED)))))))

  (define/public (check id on?)
    (with-item
     id
     (lambda (i pos)
       (send i set-check hmenu pos on?))))

  (define/public (checked? id)
    (with-item
     id
     (lambda (i pos)
       (send i get-check hmenu pos))))

  (define/public (delete-by-position pos)
    (RemoveMenu hmenu pos MF_BYPOSITION))

  (define/public (delete id)
    (with-item
     id
     (lambda (i pos)
       (RemoveMenu hmenu pos MF_BYPOSITION))))

  (public [append-item append])
  (define (append-item id label help-str-or-submenu chckable?)
    (let ([i (id-to-menu-item id)])
      (when i
        (let ([id (send i set-parent this label chckable?
                        (and (help-str-or-submenu . is-a? . menu%)
                             help-str-or-submenu))])
          (atomically
           (set! items (append items (list i)))
           (AppendMenuW hmenu (bitwise-ior MF_STRING) (cast id _long _pointer) label))))))

  (define/public (append-separator) 
    (atomically
     (set! items (append items (list #f)))
     (AppendMenuW hmenu MF_SEPARATOR #f #f)))

  (super-new))
