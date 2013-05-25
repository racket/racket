#lang racket/base
(require racket/class
         racket/draw/private/local
         racket/draw/private/dc
         racket/draw/unsafe/cairo
         racket/draw/private/bitmap
         racket/draw/private/bitmap-dc
         racket/draw/private/record-dc
         racket/draw/private/ps-setup
         ffi/unsafe
         ffi/unsafe/alloc
         "../common/queue.rkt"
         "widget.rkt"
         "utils.rkt"
         "types.rkt")

(provide 
 (protect-out printer-dc%
              show-print-setup))

(define GTK_UNIT_POINTS 1)

(define GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG 0)

(define GTK_PRINT_OPERATION_RESULT_ERROR 0)
(define GTK_PRINT_OPERATION_RESULT_APPLY 1)
(define GTK_PRINT_OPERATION_RESULT_CANCEL 2)
(define GTK_PRINT_OPERATION_RESULT_IN_PROGRESS 3)

(define GTK_PAGE_ORIENTATION_PORTRAIT 0)
(define GTK_PAGE_ORIENTATION_LANDSCAPE 1)
(define GTK_PAGE_ORIENTATION_REVERSE_PORTRAIT 2)
(define GTK_PAGE_ORIENTATION_REVERSE_LANDSCAPE 3)

(define _GtkPageSetup (_cpointer/null 'GtkPageSetup))
(define _GtkPrintSettings (_cpointer/null 'GtkPrintSettings))
(define _GtkPrintOperation _GtkWidget) ; not really, but we connect signals...
(define _GtkPrintContext (_cpointer/null 'GtkPrintContext))

(define-gtk gtk_page_setup_new (_fun -> _GtkPageSetup)
  #:wrap (allocator gobject-unref))
(define-gtk gtk_page_setup_copy (_fun _GtkPageSetup -> _GtkPageSetup)
  #:wrap (allocator gobject-unref))
(define allocated-page-setup ((allocator gobject-unref) values))

(define-gtk gtk_print_settings_new (_fun -> _GtkPrintSettings)
  #:wrap (allocator gobject-unref))

(define-gtk gtk_page_setup_get_paper_height (_fun _GtkPageSetup _int -> _double))
(define-gtk gtk_page_setup_get_paper_width (_fun _GtkPageSetup _int -> _double))
(define-gtk gtk_page_setup_get_left_margin (_fun _GtkPageSetup _int -> _double))
(define-gtk gtk_page_setup_get_right_margin (_fun _GtkPageSetup _int -> _double))
(define-gtk gtk_page_setup_get_top_margin (_fun _GtkPageSetup _int -> _double))
(define-gtk gtk_page_setup_get_bottom_margin (_fun _GtkPageSetup _int -> _double))

(define-gtk gtk_page_setup_get_orientation (_fun _GtkPageSetup -> _int))
(define-gtk gtk_page_setup_set_orientation (_fun _GtkPageSetup _int -> _void))

(define-gtk gtk_print_operation_new (_fun -> _GtkPrintOperation)
  #:wrap (allocator gobject-unref))

(define-gtk gtk_print_operation_set_default_page_setup (_fun _GtkPrintOperation _GtkPageSetup
                                                             -> _void))
(define-gtk gtk_print_operation_run (_fun _GtkPrintOperation
                                          _int
                                          (_or-null _GtkWindow)
                                          (_ptr o _pointer)
                                          -> _int))

(define-gtk gtk_print_operation_set_allow_async (_fun _GtkPrintOperation _gboolean -> _void))
(define-gtk gtk_print_operation_set_n_pages (_fun _GtkPrintOperation _int -> _void))

(define-gtk gtk_print_context_get_cairo_context (_fun _GtkPrintContext -> _cairo_t))

(define-gtk gtk_print_run_page_setup_dialog_async (_fun (_or-null _GtkWindow)
                                                        _GtkPageSetup
                                                        _GtkPrintSettings
                                                        _fpointer
                                                        _pointer
                                                        -> _void))

(define (print-setup-done page-setup cb)
  ((ptr-ref cb _racket) page-setup))
(define print_setup_done (function-ptr print-setup-done
                                       (_fun _GtkPageSetup _pointer -> _void)))

(define (pss-install-page-setup pss page-setup)
  (gtk_page_setup_set_orientation page-setup (if (eq? (send pss get-orientation) 'landscape)
                                                 GTK_PAGE_ORIENTATION_LANDSCAPE
                                                 GTK_PAGE_ORIENTATION_PORTRAIT)))

(define (show-print-setup parent)
  (let* ([pss (current-ps-setup)]
         [page-setup (or (send pss get-native)
                         (let ([ps (gtk_page_setup_new)])
                           (send pss set-native ps gtk_page_setup_copy)
                           ps))]
         [print-settings (gtk_print_settings_new)]
         [sema (make-semaphore)]
         [done-page-setup #f]
         [cell (malloc-immobile-cell (lambda (ps)
                                       (set! done-page-setup (and ps
                                                                  (allocated-page-setup ps)))
                                       (semaphore-post sema)))])
    (pss-install-page-setup pss page-setup)
    (gtk_print_run_page_setup_dialog_async (and parent
                                                (send parent get-gtk))
                                           page-setup
                                           print-settings
                                           print_setup_done
                                           cell)
    (yield sema)
    ;; `ptr-set!'s are a hack to ensure that the objects are not GCed:
    (ptr-set! cell _racket page-setup)
    (ptr-set! cell _racket print-settings)
    (free-immobile-cell cell)
    (and done-page-setup
         (begin
           (send pss set-native done-page-setup gtk_page_setup_copy)
           (send pss set-orientation (if (member
                                          (gtk_page_setup_get_orientation done-page-setup)
                                          (list GTK_PAGE_ORIENTATION_LANDSCAPE
                                                GTK_PAGE_ORIENTATION_REVERSE_LANDSCAPE))
                                         'landscape
                                         'portrait))
           #t))))
     
(define-signal-handler connect-begin-print "begin-print"
  (_fun _GtkPrintOperation _GtkPrintContext -> _void)
  (lambda (op-gtk ctx-gtk)
    (void)))

(define-signal-handler connect-draw-page "draw-page"
  (_fun _GtkPrintOperation _GtkPrintContext _int -> _void)
  (lambda (op-gtk ctx-gtk page-no)
    (let ([wx (gtk->wx op-gtk)])
      (when wx
        (send wx draw-page ctx-gtk page-no)))))

(define-signal-handler connect-done "done"
  (_fun _GtkPrintOperation _int -> _void)
  (lambda (op-gtk res)
    (when (= res GTK_PRINT_OPERATION_RESULT_CANCEL)
      (let ([wx (gtk->wx op-gtk)])
        (when wx
          (send wx done))))))

(define-signal-handler connect-end-print "end-print"
  (_fun _GtkPrintOperation _GtkPrintContext -> _void)
  (lambda (op-gtk ctx-gtk)
    (let ([wx (gtk->wx op-gtk)])
      (when wx
        (send wx done)))))

(define printout%
  (class widget%
    (init-field op-gtk
                pages
                page-setup)
    (super-new [gtk op-gtk])

    (connect-begin-print op-gtk)
    (connect-draw-page op-gtk)
    (connect-done op-gtk)
    (connect-end-print op-gtk)

    (gtk_print_operation_set_n_pages op-gtk (length pages))
    (gtk_print_operation_set_allow_async op-gtk #t)
    (gtk_print_operation_set_default_page_setup op-gtk page-setup)

    (define done-sema (make-semaphore))

    (define/public (go)
      (let ([res (gtk_print_operation_run op-gtk
                                          GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG
                                          #f)])
        (yield done-sema)))

    (define/public (draw-page ctx-gtk pageno)
      (let ([cr (gtk_print_context_get_cairo_context ctx-gtk)])
        ((list-ref pages pageno)
         (make-object
          (class (dc-mixin default-dc-backend%)
            (super-new)
            (define orig-matrix (make-cairo_matrix_t 0.0 0.0 0.0 0.0 0.0 0.0))
            (cairo_get_matrix cr orig-matrix)
            (define/override (init-cr-matrix cr) (cairo_set_matrix cr orig-matrix))
            (define/override (get-cr) cr))))))

    (define/public (done)
      (semaphore-post done-sema))))

(define printer-dc%
  (class (record-dc-mixin (dc-mixin bitmap-dc-backend%))
    (init [parent #f])

    (super-make-object (make-object bitmap% 1 1))

    (inherit get-recorded-command
             reset-recording)

    (define pages null)
    (define/override (end-page)
      (set! pages (cons (get-recorded-command) pages))
      (reset-recording))

    (define page-setup (or (let-values ([(ps copier)
                                         (send (current-ps-setup)
                                               get-native-copy)])
                             ps)
                           (gtk_page_setup_new)))
    (pss-install-page-setup (current-ps-setup) page-setup)

    (define page-width (- (gtk_page_setup_get_paper_width page-setup GTK_UNIT_POINTS)
                          (gtk_page_setup_get_left_margin page-setup GTK_UNIT_POINTS)
                          (gtk_page_setup_get_right_margin page-setup GTK_UNIT_POINTS)))
    (define page-height (- (gtk_page_setup_get_paper_height page-setup GTK_UNIT_POINTS)
                           (gtk_page_setup_get_top_margin page-setup GTK_UNIT_POINTS)
                           (gtk_page_setup_get_bottom_margin page-setup GTK_UNIT_POINTS)))
    (define page-scaling 1.0) ; scale from gtk_print_operation_run is too late

    (define/override (get-size)
      (values (/ page-width page-scaling) (/ page-height page-scaling)))

    (define/override (get-device-scale)
      (values page-scaling page-scaling))

    (define/override (end-doc)
      (send (new printout% 
                 [op-gtk (gtk_print_operation_new)]
                 [pages (reverse pages)]
                 [page-setup page-setup])
            go)
      (void))))
