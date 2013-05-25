#lang racket/base
(require racket/class
         ffi/unsafe
         (only-in racket/list drop take)
         "../../lock.rkt"
         "../../syntax.rkt"
         "../common/event.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "menu-item.rkt")

(provide
 (protect-out menu%))

(define-user32 CreatePopupMenu (_wfun -> _HMENU))
(define-user32 AppendMenuW (_wfun _HMENU _UINT _pointer _string/utf-16 -> (r : _BOOL)
                                  -> (unless r (failed 'AppendMenuW))))
(define-user32 EnableMenuItem (_wfun _HMENU _UINT _UINT -> _BOOL))

(define-user32 TrackPopupMenu(_wfun _HMENU _UINT _int _int _int _HWND (_or-null _RECT-pointer)
                                    -> _int))

(define TPM_LEFTBUTTON  #x0000)
(define TPM_RIGHTBUTTON #x0002)
(define TPM_NONOTIFY    #x0080)
(define TPM_RETURNCMD   #x0100)

(defclass menu% object%
  (init lbl
        cb
        font)

  (define label lbl)
  (define parent #f)
  (define items null)

  (define callback cb)

  (define hmenu (CreatePopupMenu))

  (define/public (get-hmenu) hmenu)

  (define/public (set-parent p lbl parent-hmenu)
    (set! label lbl)
    (set! parent p)
    (AppendMenuW parent-hmenu 
                 (bitwise-ior MF_POPUP MF_STRING)
                 hmenu
                 lbl))

  (define/public (select mb)
    (when parent
      (let ([m (regexp-match #rx"&[^&]" label)])
        (when m
          (send parent popup-menu-with-char (string-ref (car m) 1)))))
    #t)

  (def/public-unimplemented get-font)
  (def/public-unimplemented set-width)
  (def/public-unimplemented set-title)

  (define/public (popup gx gy hwnd call-callback)
    (let ([cmd (TrackPopupMenu hmenu
                               (bitwise-ior
                                TPM_LEFTBUTTON
                                TPM_RIGHTBUTTON
                                TPM_NONOTIFY
                                TPM_RETURNCMD)
                               gx gy
                               0 hwnd #f)])
      (let* ([e (new popup-event% [event-type 'menu-popdown])])
        (unless (zero? cmd)
          (send e set-menu-id cmd))
        (call-callback (lambda () (callback this e))))))

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

  (define/public (set-help-string id str)
    (void))

  (define/public (number) (length items))

  (define/public (enable id on?)
    (with-item
     id
     (lambda (i pos)
       (void
        (EnableMenuItem hmenu pos 
                        (bitwise-ior MF_BYPOSITION
                                     (if on? MF_ENABLED MF_GRAYED)))))))

  (define/public (enable-self parent-hmenu pos on?)
    (EnableMenuItem parent-hmenu pos  
                    (bitwise-ior MF_BYPOSITION
                                 (if on? MF_ENABLED MF_GRAYED))))

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

  (define/public (auto-check id)
    (with-item
     id
     (lambda (i pos)
       (send i set-check hmenu pos (not (send i get-check hmenu pos))))))

  (define/private (remove-item! pos)
    (set! items 
          (append (take items pos)
                  (drop items (add1 pos)))))

  (define/public (delete-by-position pos)
    (atomically
     (remove-item! pos)
     (RemoveMenu hmenu pos MF_BYPOSITION)))

  (define/public (delete id)
    (with-item
     id
     (lambda (i pos)
       (atomically
        (remove-item! pos)
        (RemoveMenu hmenu pos MF_BYPOSITION)))))

  (public [append-item append])
  (define (append-item id label help-str-or-submenu chckable?)
    (let ([i (id-to-menu-item id)])
      (when i
        (let* ([submenu (and (help-str-or-submenu . is-a? . menu%)
                             help-str-or-submenu)]
               [id (send i set-parent this label chckable?
                         submenu)])
          (atomically
           (set! items (append items (list i)))
           (if submenu
               (AppendMenuW hmenu (bitwise-ior MF_POPUP MF_STRING) (send submenu get-hmenu) label)
               (AppendMenuW hmenu (bitwise-ior MF_STRING) (cast id _intptr _pointer) label)))))))

  (define/public (append-separator) 
    (atomically
     (set! items (append items (list #f)))
     (AppendMenuW hmenu MF_SEPARATOR #f #f)))

  (super-new))
