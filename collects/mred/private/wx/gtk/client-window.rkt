#lang scheme/base
(require scheme/foreign
         scheme/class
         "../../syntax.rkt"
         "widget.rkt"
         "window.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt")
(unsafe!)

(provide client-size-mixin)

;; ----------------------------------------

(define-signal-handler connect-size-allocate "size-allocate"
  (_fun _GtkWidget _GtkAllocation-pointer -> _gboolean)
  (lambda (gtk a)
    (let ([wx (gtk->wx gtk)])
      (send wx remember-client-size 
            (GtkAllocation-x a)
            (GtkAllocation-y a)
            (GtkAllocation-width a)
            (GtkAllocation-height a)))
    #t))

(define (client-size-mixin %)
  (class %
    (init client-gtk)

    (connect-size-allocate client-gtk)

    (define client-w 0)
    (define client-h 0)
    (define client-x 0)
    (define client-y 0)

    (define/public (on-client-size w h) (void))

    (define/public (remember-client-size x y w h)
      ;; Called in the Gtk event-loop thread
      (set! client-x x)
      (set! client-y y)
      (set! client-w w)
      (set! client-h h)
      (queue-window-event this (lambda () 
				 (internal-on-client-size w h)
				 (on-client-size w h))))

    (define/public (internal-on-client-size w h)
      (void))

    (define/override (get-client-size xb yb)
      (set-box! xb client-w)
      (set-box! yb client-h))

    (define/override (get-client-delta)
      (values client-x client-y))

    (super-new)))