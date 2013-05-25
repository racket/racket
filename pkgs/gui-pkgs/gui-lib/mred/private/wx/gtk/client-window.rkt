#lang racket/base
(require ffi/unsafe
         racket/class
         "../../syntax.rkt"
         "widget.rkt"
         "window.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt")

(provide 
 (protect-out client-size-mixin))

;; ----------------------------------------

(define-signal-handler connect-size-allocate "size-allocate"
  (_fun _GtkWidget _GtkAllocation-pointer -> _gboolean)
  (lambda (gtk a)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx save-client-size 
              (GtkAllocation-x a)
              (GtkAllocation-y a)
              (GtkAllocation-width a)
              (GtkAllocation-height a))))
    #t))

(define (client-size-mixin %)
  (class %
    (init client-gtk)

    (connect-size-allocate client-gtk)

    (define client-x 0)
    (define client-y 0)

    (define/public (on-client-size w h) (void))

    (define client-size-key #f)

    (define/public (save-client-size x y w h)
      ;; Called in the Gtk event-loop thread
      (set! client-x x)
      (set! client-y y)
      (when client-size-key (set-box! client-size-key #f))
      (let ([key (box #t)])
        (set! client-size-key key)
        (queue-window-event this (lambda () 
                                   (when (unbox key)
                                     (internal-on-client-size w h)
                                     (on-client-size w h))))))

    (define/public (internal-on-client-size w h)
      (void))

    (define/override (get-client-delta)
      (values client-x client-y))

    (super-new)))
