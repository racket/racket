#lang racket/base
(require racket/class
         ffi/unsafe
          "../../syntax.rkt"
         "window.rkt"
         "client-window.rkt"
         "utils.rkt"
         "panel.rkt"
         "types.rkt"
         "widget.rkt"
         "message.rkt"
         "../../lock.rkt"
         "../common/event.rkt")

(provide 
 (protect-out tab-panel%))

(define-gtk gtk_notebook_new (_fun -> _GtkWidget))

(define-gtk gtk_notebook_append_page (_fun _GtkWidget _GtkWidget (_or-null _GtkWidget) -> _void))
(define-gtk gtk_notebook_remove_page (_fun _GtkWidget _int -> _void))
(define-gtk gtk_notebook_set_scrollable (_fun _GtkWidget _gboolean -> _void))
(define-gtk gtk_notebook_get_current_page (_fun _GtkWidget -> _int))
(define-gtk gtk_notebook_set_current_page (_fun _GtkWidget _int -> _void))
(define-gtk gtk_notebook_get_tab_label  (_fun _GtkWidget _GtkWidget -> _GtkWidget))

(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))

(define-gtk gtk_widget_ref (_fun _GtkWidget -> _void))
(define-gtk gtk_widget_unref (_fun _GtkWidget -> _void))

(define-struct page (bin-gtk label-gtk))

(define-signal-handler connect-changed "switch-page"
  (_fun _GtkWidget _pointer _int -> _void)
  (lambda (gtk ignored i)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx page-changed i)))))

(define tab-panel%
  (class (client-size-mixin (panel-container-mixin (panel-mixin window%)))
    (init parent
          x y w h
          style
          labels)
    
    (inherit set-size set-auto-size infer-client-delta get-gtk
             reset-child-dcs get-height)

    (define gtk (gtk_notebook_new))
    ;; Reparented so that it's always in the current page's bin:
    (define client-gtk (gtk_fixed_new))

    (gtk_notebook_set_scrollable gtk #t)

    (super-new [parent parent]
               [gtk gtk]
               [client-gtk client-gtk]
               [extra-gtks (list client-gtk)]
               [no-show? (memq 'deleted style)])

    ; Once without tabs to set client-width delta:
    (infer-client-delta #t #f)
    
    (define empty-bin-gtk (gtk_hbox_new #f 0))
    (define current-bin-gtk #f)

    (define (select-bin bin-gtk)
      (set! current-bin-gtk bin-gtk)
      (gtk_box_pack_start bin-gtk client-gtk #t #t 0)
      ;; re-parenting can change the underlying window dc:
      (reset-child-dcs))

    (define pages
      (for/list ([lbl labels])
        (let ([bin-gtk (gtk_hbox_new #f 0)]
              [label-gtk (gtk_label_new_with_mnemonic lbl)])
          (gtk_notebook_append_page gtk bin-gtk label-gtk)
          (gtk_widget_show bin-gtk)
          (make-page bin-gtk label-gtk))))

    (define/private (install-empty-page)
      (gtk_notebook_append_page gtk empty-bin-gtk #f)
      (gtk_widget_show empty-bin-gtk))

    (if (null? pages)
        (begin
          (select-bin empty-bin-gtk)
          (install-empty-page))
        (begin
          (select-bin (page-bin-gtk (car pages)))))
    (gtk_widget_show client-gtk)
    
    (connect-key-and-mouse gtk)
    (connect-focus gtk)

    ; With tabs to set client-width delta:
    (infer-client-delta #f #t)

    (set-auto-size)

    (define callback void)
    (define/public (set-callback cb) (set! callback cb))
    (define/private (do-callback)
      (callback this (new control-event%
                          [event-type 'tab-panel]
                          [time-stamp (current-milliseconds)])))

    (define/public (swap-in bin-gtk)
      (gtk_widget_ref client-gtk)
      (gtk_container_remove current-bin-gtk client-gtk)
      (select-bin bin-gtk)
      (gtk_widget_unref client-gtk))

    (define callback-ok? #t)

    (define/public (page-changed i)
      ; range check works around spurious callbacks:
      (when (< -1 i (length pages))
        (swap-in (page-bin-gtk (list-ref pages i)))
        (when callback-ok?
          (queue-window-event this (lambda () (do-callback))))))
    (connect-changed gtk)
    
    (define/override (get-client-gtk) client-gtk)

    (public [append* append])
    (define (append* lbl)
      (atomically
       (set! callback-ok? #f)
       (do-append lbl)
       (set! callback-ok? #t)))

    (define/private (do-append lbl)
      (let ([page
             (let ([bin-gtk (gtk_hbox_new #f 0)]
                   [label-gtk (gtk_label_new_with_mnemonic lbl)])
               (gtk_notebook_append_page gtk bin-gtk label-gtk)
               (gtk_widget_show bin-gtk)
               (make-page bin-gtk label-gtk))])
        (set! pages (append pages (list page)))
        (when (null? (cdr pages))
          (swap-in (page-bin-gtk (car pages)))
          (g_object_ref empty-bin-gtk)
          (gtk_notebook_remove_page gtk 0))))

    (define/private (do-delete i)
      (let ([page (list-ref pages i)])
        (when (ptr-equal? current-bin-gtk (page-bin-gtk page))
          (let ([cnt (length pages)])
            (if (= i (sub1 cnt))
                (if (null? (cdr pages))
                    (begin
                      (install-empty-page)
                      (set! pages null)
                      (gtk_notebook_set_current_page gtk 1)
                      (swap-in empty-bin-gtk))
                    (gtk_notebook_set_current_page gtk (sub1 i)))
                (gtk_notebook_set_current_page gtk (add1 i)))))
        (gtk_notebook_remove_page gtk i)
        (set! pages (remq page pages))))

    (define/public (delete i)
      (atomically
       (set! callback-ok? #f)
       (do-delete i)
       (set! callback-ok? #t)))

    (define/public (set choices)
      (atomically
       (set! callback-ok? #f)
       (for ([page (in-list pages)])
         (do-delete 0))
       (for ([lbl (in-list choices)])
         (append* lbl))
       (set! callback-ok? #t)))

    (define/public (set-label i str)
      (gtk_label_set_text_with_mnemonic (page-label-gtk (list-ref pages i)) 
                                        (mnemonic-string str)))

    (define/public (number) (length pages))
    (define/public (button-focus n)
      (if (= n -1)
          (get-selection)
          (direct-set-selection n)))

    (define/override (gets-focus?) #t)
    (define/override (set-focus)
      (gtk_widget_grab_focus gtk))

    (define/private (direct-set-selection i)
      (gtk_notebook_set_current_page gtk i))
    (define/public (set-selection i)
      (atomically
       (set! callback-ok? #f)
       (direct-set-selection i)
       (set! callback-ok? #t)))
    (define/public (get-selection)
      (gtk_notebook_get_current_page gtk))))
