#lang racket/base
(require ffi/unsafe
         racket/class
         "types.rkt"
         "utils.rkt"
         "widget.rkt"
         "queue.rkt"
         "../common/queue.rkt")

(provide 
 (protect-out show-dialog
              _GtkResponse))

(define _GtkResponse
  (_enum 
   '(none = -1
     reject = -2
     accept = -3
     delete-event = -4
     ok = -5
     cancel = -6
     close = -7
     yes = -8
     no = -9
     apply = -10
     help = -11)
   _fixint))

(define-signal-handler connect-response "response"
  (_fun _GtkWidget _GtkResponse _pointer -> _void)
  (lambda (gtk id data)
    (let* ([p (ptr-ref data _racket)]
           [response-sema (car p)]
           [response-box (cdr p)])
      (set-box! response-box id)
      (semaphore-post response-sema))))

(define (show-dialog dlg-gtk
                     [validate? (lambda (val) #t)])
  (let* ([response-sema (make-semaphore)]
         [response-box (box #f)]
         [cell (malloc-immobile-cell (cons response-sema
                                           response-box))]
         [es (current-eventspace)])
    (connect-response dlg-gtk cell)
    (eventspace-adjust-external-modal! es 1)
    (gtk_widget_show dlg-gtk)
    (let loop ()
      (yield response-sema)
      (unless (validate? (unbox response-box))
        (loop)))
    (eventspace-adjust-external-modal! es -1)
    (free-immobile-cell cell) ;; FIXME : don't leak
    (gtk_widget_hide dlg-gtk)
    (unbox response-box)))
