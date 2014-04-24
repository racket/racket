#lang racket/base
(require ffi/unsafe
         racket/class
         "../../syntax.rkt"
         "../../lock.rkt"
         "../common/queue.rkt"
         "queue.rkt"
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out widget%
              gtk->wx

              gtk_widget_show
              gtk_widget_hide
              gtk_widget_destroy

              gtk_vbox_new
              gtk_hbox_new
              gtk_box_pack_start
              gtk_box_pack_end))

(define-gtk gtk_widget_show (_fun _GtkWidget -> _void))
(define-gtk gtk_widget_hide (_fun _GtkWidget -> _void))

(define-gtk gtk_widget_destroy (_fun _pointer -> _void))

(define-gtk gtk_vbox_new (_fun _gboolean _int -> _GtkWidget))
(define-gtk gtk_hbox_new (_fun _gboolean _int -> _GtkWidget))
(define-gtk gtk_box_pack_start (_fun _GtkWidget _GtkWidget _gboolean _gboolean _uint -> _void))
(define-gtk gtk_box_pack_end (_fun _GtkWidget _GtkWidget _gboolean _gboolean _uint -> _void))
(define-gtk gtk_widget_get_parent (_fun _GtkWidget -> (_or-null _GtkWidget)))

(define-signal-handler connect-destroy "destroy"
  (_fun _GtkWidget _pointer -> _void)
  (lambda (gtk cell)
    (free-immobile-cell cell)))

(define widget%
  (class object%
    (init gtk
          [extra-gtks null]
          [parent #f])
    (init-field [eventspace (if parent
                                (send parent get-eventspace)
                                (current-eventspace))])

    (when (eventspace-shutdown? eventspace)
      (error '|GUI object initialization| "the eventspace has been shutdown"))

    (define/public (get-eventspace) eventspace)
    (define/public (direct-update?) #t)

    (define/public (install-widget-parent p)
      (set! eventspace (send p get-eventspace)))
    
    (super-new)

    (define/public (register-extra-gtk gtk extra-gtk)
      (define cell (g_object_get_data gtk "wx"))
      (g_object_set_data extra-gtk "wx" cell))

    (atomically
     (let ([cell (malloc-immobile-cell (make-weak-box this))])
       (g_object_set_data gtk "wx" cell)
       (for ([gtk (in-list extra-gtks)])
         (g_object_set_data gtk "wx" cell))
       (connect-destroy gtk cell)))))

(define (gtk->wx gtk)
  (let ([ptr (g_object_get_data gtk "wx")])
    (and ptr 
         (let ([wb (ptr-ref ptr _scheme)])
           (and wb (weak-box-value wb))))))

(set-widget-hook! (lambda (gtk)
                    (let loop ([gtk gtk])
                      (and gtk
                           (let ([wx (gtk->wx gtk)])
                             (or wx
                                 (loop (gtk_widget_get_parent gtk))))))))

