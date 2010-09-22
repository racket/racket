#lang scheme/base
(require ffi/unsafe
         scheme/class
          "../../syntax.rkt")

(provide menu-item%
         id-to-menu-item)

;; Menu itens are identified by 16-bit numbers, so we have
;;  to keep a hash mapping them to menu items.
(define ids (make-hash))

(define (id-to-menu-item id)
  (let ([wb (hash-ref ids id #f)])
    (and wb (weak-box-value wb))))

(defclass menu-item% object%

  (define id
    (let loop ()
      (let ([id (add1 (random #x7FFE))])
        (let ([wb (hash-ref ids id #f)])
          (if (and wb
                   (weak-box-value wb))
              (loop)
              (begin
                (hash-set! ids id (make-weak-box this))
                id))))))

  (define parent #f)
  (define label #f)
  (define checkable? #f)

  (define/public (set-parent p lbl chkbl?)
    (set! parent p)
    (set! label lbl)
    (set! checkable? chkbl?)
    id)

  (public [get-id id])
  (define (get-id) id)

  (super-new))
