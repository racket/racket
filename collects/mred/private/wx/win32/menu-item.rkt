#lang racket/base
(require ffi/unsafe
         racket/class
         "utils.rkt"
         "types.rkt"
         "const.rkt"
          "../../lock.rkt"
          "../../syntax.rkt")

(provide
 (protect-out menu-item%
              id-to-menu-item))

;; Menu itens are identified by 16-bit numbers, so we have
;;  to keep a hash mapping them to menu items.
(define ids (make-hash))

(define (id-to-menu-item id)
  (let ([wb (atomically (hash-ref ids id #f))])
    (and wb (weak-box-value wb))))

(defclass menu-item% object%

  (define id
    (let loop ()
      (let ([id (add1 (random #x7FFE))])
        (let ([wb (atomically (hash-ref ids id #f))])
          (if (and wb
                   (weak-box-value wb))
              (loop)
              (begin
                (atomically (hash-set! ids id (make-weak-box this)))
                id))))))

  (define parent #f)
  (define label #f)
  (define checkable? #f)
  (define submenu #f)

  (define/public (set-parent p lbl chkbl? subm)
    (set! parent p)
    (set! label lbl)
    (set! checkable? chkbl?)
    (set! submenu subm)
    id)

  (define/public (set-label hmenu pos str)
    (if submenu
        (ModifyMenuW hmenu pos
                     (bitwise-ior MF_BYPOSITION MF_STRING MF_POPUP)
                     (cast (send submenu get-hmenu) _HMENU _UINT_PTR)
                     str)
        (ModifyMenuW hmenu pos
                     (bitwise-ior MF_BYPOSITION MF_STRING 
                                  (GetMenuState hmenu pos MF_BYPOSITION))
                     id
                     str)))

  (define/public (set-check hmenu pos on?)
    (void
     (CheckMenuItem hmenu pos (bitwise-ior MF_BYPOSITION
                                           (if on?
                                               MF_CHECKED
                                               MF_UNCHECKED)))))

  (define/public (get-check hmenu pos)
    (let ([s (GetMenuState hmenu pos MF_BYPOSITION)])
      (not (zero? (bitwise-and s MF_CHECKED)))))

  (define/public (auto-check)
    (when checkable?
      (send parent auto-check id)))

  (public [get-id id])
  (define (get-id) id)

  (super-new))
