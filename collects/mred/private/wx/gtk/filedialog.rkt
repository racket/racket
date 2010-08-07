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
         "../common/handlers.rkt"
         "../common/queue.rkt")

(provide file-selector)

(define _GtkFileChooserDialog _GtkWidget)
(define _GtkFileChooser (_cpointer 'GtkFileChooser))
(define _GtkFileChooserAction 
  (_enum (list 'open 'save 'select-folder 'create-folder)))

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
  (define dlg (gtk_file_chooser_dialog_new 
               message (and parent (send parent get-gtk))
               (case type
                 [(dir) 'select-directory]
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
               #f))
  (when (eq? 'multi type)
    (gtk_file_chooser_set_select_multiple dlg #t))
  (when filename
    (gtk_file_chooser_set_current_name dlg filename))
  (when directory
    (gtk_file_chooser_set_current_folder dlg directory))
  (for ([f (in-list filters)])
    (match f
      [(list name glob)
       (let ([ff (gtk_file_filter_new)])
         (gtk_file_filter_set_name ff name)
         (gtk_file_filter_add_pattern ff glob)
         (gtk_file_chooser_add_filter dlg ff))]))
  (define ans (and (= -3 (show-dialog dlg
                                      (lambda (v)
                                        (or (not (= v -3))
                                            ;; FIXME: for get mode, probably should check file vs.
                                            ;;  directory name
                                            (not (eq? type 'put))
                                            (not (file-exists? (gtk_file_chooser_get_filename dlg)))
                                            ;; FIXME: need to ask "replace the file? here
                                            #t))))
                   (if (eq? type 'multi)
                       (gtk_file_chooser_get_filenames dlg)
                       (gtk_file_chooser_get_filename dlg))))
  (gtk_widget_destroy dlg)
  ans)

(define-gtk gtk_main_iteration_do (_fun _gboolean -> _gboolean))

(define-signal-handler connect-response "response"
  (_fun _GtkWidget _int _pointer -> _void)
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
                                           response-box))])
    (connect-response dlg-gtk cell)
    (gtk_widget_show dlg-gtk)
    (let loop ()
      (yield response-sema)
      (unless (validate? (unbox response-box))
        (loop)))
    (free-immobile-cell cell) ;; FIXME : don't leak
    (gtk_widget_hide dlg-gtk)
    (unbox response-box)))

(define (id-to-menu-item i) i)
(define-unimplemented get-the-x-selection)
(define-unimplemented get-the-clipboard)
(define-unimplemented show-print-setup)
(define (can-show-print-setup?) #f)
