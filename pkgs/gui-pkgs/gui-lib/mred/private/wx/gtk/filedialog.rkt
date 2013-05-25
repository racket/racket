#lang racket/base
(require ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         racket/class
         racket/match
         "types.rkt"
         "utils.rkt"
         "widget.rkt"
         "queue.rkt"
         "stddialog.rkt"
         "../common/handlers.rkt"
         "../common/queue.rkt")

(provide 
 (protect-out file-selector))

(define _GtkFileChooserDialog _GtkWidget)
(define _GtkFileChooser (_cpointer 'GtkFileChooser))
(define _GtkFileChooserAction 
  (_enum (list 'open 'save 'select-folder 'create-folder)))

;; FIXME: really there are varargs here, but we don't need them for
;; our purposes
(define-gtk gtk_file_chooser_dialog_new 
  (_fun _string (_or-null _GtkWindow) 
        _GtkFileChooserAction
        _string _GtkResponse
        _string _GtkResponse
        (_or-null _pointer)
        -> _GtkFileChooserDialog))
;; FIXME - should really be _GtkDialog but no subtyping
(define-gtk gtk_dialog_run (_fun _GtkFileChooserDialog -> _int))
;; FIXME ;; these should really be _GtkFileChooser but no subtyping
(define-gtk gtk_file_chooser_get_filename 
  (_fun _GtkFileChooserDialog -> _gpath/free))
(define-gtk gtk_file_chooser_get_filenames
  (_fun _GtkFileChooserDialog -> (_GSList _gpath/free)))
(define-gtk gtk_file_chooser_set_current_name
  (_fun _GtkFileChooserDialog _path -> _void))
(define-gtk gtk_file_chooser_set_current_folder
  (_fun _GtkFileChooserDialog _path -> _void))
(define-gtk gtk_file_chooser_set_do_overwrite_confirmation
  (_fun _GtkFileChooserDialog _gboolean -> _void))
(define-gtk gtk_file_chooser_set_select_multiple
  (_fun _GtkFileChooserDialog _gboolean -> _void))

(define _GtkFileFilter (_cpointer 'GtkFileFilter))
(define-gtk gtk_file_filter_new (_fun -> _GtkFileFilter))
(define-gtk gtk_file_filter_set_name
  (_fun _GtkFileFilter _string -> _void))
(define-gtk gtk_file_filter_add_pattern
  (_fun _GtkFileFilter _string -> _void))

(define-gtk gtk_file_chooser_add_filter
  (_fun _GtkFileChooserDialog _GtkFileFilter -> _void))

(define (file-selector message directory filename 
                       extension ;; always ignored
                       filters style parent)
  (define type (car style)) ;; the rest of `style' is irrelevant on Gtk
  (define dlg (as-gtk-window-allocation
               (gtk_file_chooser_dialog_new 
                message (and parent (send parent get-gtk))
                (case type
                  [(dir) 'select-folder]
                  [(put) 'save]
                  [else 'open])
                "gtk-cancel" 'cancel
                ;; no stock names for "Select"
                (case type
                  [(dir) "Choose"]
                  [(put) "gtk-save"]
                  [(get) "gtk-open"]
                  [(multi) "Choose"])
                'accept
                #f)))
  (when (eq? 'multi type)
    (gtk_file_chooser_set_select_multiple dlg #t))
  (when filename
    (gtk_file_chooser_set_current_name dlg filename))
  (when directory
    (gtk_file_chooser_set_current_folder dlg directory))
  (when (eq? 'put type)
    (gtk_file_chooser_set_do_overwrite_confirmation dlg #t))
  (for ([f (in-list filters)])
    (match f
      [(list name glob)
       (let ([ff (gtk_file_filter_new)]
             [glob (if (equal? glob "*.*") "*" glob)])
         (gtk_file_filter_set_name ff (if (equal? glob "*")
                                          name
                                          (format "~a (~a)" name glob)))
         (for ([glob (in-list (regexp-split #rx" *; *" glob))]
               #:when ((string-length glob) . > . 0))
           (gtk_file_filter_add_pattern ff glob))
         (gtk_file_chooser_add_filter dlg ff))]))
  (define ans (and (eq? 'accept (show-dialog dlg))
                   (if (eq? type 'multi)
                       (gtk_file_chooser_get_filenames dlg)
                       (gtk_file_chooser_get_filename dlg))))
  (gtk_widget_destroy dlg)
  ans)

(define-gtk gtk_main_iteration_do (_fun _gboolean -> _gboolean))
