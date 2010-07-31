#lang racket/base
(require ffi/unsafe
         racket/class
         racket/draw
         ffi/unsafe/alloc
         racket/draw/color
         "../../syntax.rkt"
         "../common/event.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "client-window.rkt"
         "widget.rkt"
         "dc.rkt")

(provide canvas%)

;; ----------------------------------------

(define-gtk gtk_drawing_area_new (_fun -> _GtkWidget))

(define-gtk gtk_combo_box_entry_new_text (_fun -> _GtkWidget))
(define-gtk gtk_combo_box_append_text (_fun _GtkWidget _string -> _void))

(define-gtk gtk_widget_queue_draw (_fun _GtkWidget -> _void))

(define-gtk gtk_hscrollbar_new (_fun _pointer -> _GtkWidget))
(define-gtk gtk_vscrollbar_new (_fun _pointer -> _GtkWidget))

(define _GtkAdjustment _GtkWidget) ; no, actually a GtkObject
(define-gtk gtk_adjustment_new (_fun _double* _double* _double* _double* _double* _double* -> _GtkAdjustment))
(define-gtk gtk_adjustment_configure (_fun _GtkAdjustment _double* _double* _double* _double* _double* _double* -> _void))
(define-gtk gtk_adjustment_get_value (_fun _GtkAdjustment -> _double*))
(define-gtk gtk_adjustment_set_value (_fun _GtkAdjustment _double* -> _void))
(define-gtk gtk_adjustment_get_upper (_fun _GtkAdjustment -> _double*))
(define-gtk gtk_adjustment_set_upper (_fun _GtkAdjustment _double* -> _void))
(define-gtk gtk_adjustment_get_page_size (_fun _GtkAdjustment -> _double*))
(define-gtk gtk_adjustment_set_page_size (_fun _GtkAdjustment _double* -> _void))
(define-gtk gtk_adjustment_get_page_increment (_fun _GtkAdjustment -> _double*))
(define-gtk gtk_adjustment_set_page_increment (_fun _GtkAdjustment _double* -> _void))

(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))

(define-gobj g_object_set_bool (_fun _GtkWidget _string _gboolean [_pointer = #f] -> _void)
  #:c-id g_object_set)

(define-cstruct _GdkColor ([pixel _uint32]
                           [red _uint16]
                           [green _uint16]
                           [blue _uint16]))

(define-gdk gdk_gc_unref (_fun _pointer -> _void)
  #:wrap (deallocator))
(define-gdk gdk_gc_new (_fun _GdkWindow -> _pointer)
  #:wrap (allocator gdk_gc_unref))
(define-gdk gdk_gc_set_rgb_fg_color (_fun _pointer _GdkColor-pointer -> _void))
(define-gdk gdk_draw_rectangle (_fun _GdkWindow _pointer _gboolean _int _int _int _int -> _void))

;; We rely some on the implementation of GtkComboBoxEntry to replace
;; the drawing routine.
(define-cstruct _GList ([data _pointer]))
(define-gdk gdk_window_get_children (_fun _pointer -> _GList-pointer/null))
(define-gdk gdk_window_hide (_fun _pointer -> _void))
(define (get-subwindow gtk)
  (let* ([win (g_object_get_window gtk)]
         [subs (gdk_window_get_children win)])
    (if subs
        (GList-data subs)
        win)))

(define-signal-handler connect-changed "changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (send wx combo-maybe-clicked))))

(define-gtk gtk_combo_box_set_active (_fun _GtkWidget _int -> _void))
(define-gtk gtk_combo_box_get_active (_fun _GtkWidget -> _int))

(define (handle-expose gtk event)
  (let ([wx (gtk->wx gtk)])
    (let ([gc (send wx get-canvas-background-for-clearing)])      
      (when gc
        (gdk_draw_rectangle (g_object_get_window gtk) gc #t
                            0 0 32000 32000)))
    (send wx queue-paint))
  #t)
(define handle_expose
  (function-ptr handle-expose (_fun #:atomic? #t _GtkWidget _GdkEventExpose -> _gboolean)))

(define (handle-value-changed-h gtk ignored)
  (let ([wx (gtk->wx gtk)])
    (queue-window-event wx (lambda () (send wx do-scroll 'horizontal))))
  #t)
(define handle_value_changed_h
  (function-ptr handle-value-changed-h (_fun #:atomic? #t _GtkWidget _pointer -> _void)))

(define (handle-value-changed-v gtk ignored)
  (let ([wx (gtk->wx gtk)])
    (queue-window-event wx (lambda () (send wx do-scroll 'vertical))))
  #t)
(define handle_value_changed_v
  (function-ptr handle-value-changed-v (_fun #:atomic? #t _GtkWidget _pointer -> _void)))

(define-gtk gtk_entry_get_type (_fun -> _GType))

(define canvas%
  (class (client-size-mixin window%)
    (init parent
          x y w h
          style
          [ignored-name #f]
          [gl-config #f])

    (inherit get-gtk set-size get-size get-client-size 
             on-size register-as-child get-top-win)

    (define is-combo? (memq 'combo style))

    (define-values (client-gtk gtk hscroll-adj vscroll-adj hscroll-gtk vscroll-gtk resize-box)
      (cond
       [(or (memq 'hscroll style)
            (memq 'vscroll style))
        (let* ([client-gtk (gtk_drawing_area_new)]
               [hadj (gtk_adjustment_new 0.0 0.0 1.0 1.0 1.0 1.0)]
               [vadj (gtk_adjustment_new 0.0 0.0 1.0 1.0 1.0 1.0)])
          (let ([h (gtk_hbox_new #f 0)]
                [v (gtk_vbox_new #f 0)]
                [v2 (gtk_vbox_new #f 0)]
                [h2 (gtk_vbox_new #f 0)]
                [hscroll (gtk_hscrollbar_new hadj)]
                [vscroll (gtk_vscrollbar_new vadj)]
                [resize-box (gtk_drawing_area_new)])
            (gtk_box_pack_start h v #t #t 0)
            (gtk_box_pack_start v client-gtk #t #t 0)
            (gtk_box_pack_start h v2 #f #f 0)
            (gtk_box_pack_start v2 vscroll #t #t 0)
            (gtk_box_pack_start v h2 #f #f 0)
            (gtk_box_pack_start h2 hscroll #t #t 0)
            (gtk_box_pack_start v2 resize-box #f #f 0)
            (gtk_widget_show hscroll)
            (gtk_widget_show vscroll)
            (gtk_widget_show h)
            (gtk_widget_show v)
            (gtk_widget_show v2)
            (gtk_widget_show h2)
            (gtk_widget_show resize-box)
            (gtk_widget_show client-gtk)
            (values client-gtk h hadj vadj h2 v2 resize-box)))]
       [is-combo?
        (let* ([gtk (gtk_combo_box_entry_new_text)]
               [orig-entry (gtk_bin_get_child gtk)])
          (values orig-entry gtk #f #f #f #f #f))]
       [else
        (let ([client-gtk (gtk_drawing_area_new)])
          (values client-gtk client-gtk #f #f #f #f #f))]))

    (super-new [parent parent]
               [gtk gtk]
               [client-gtk client-gtk]
               [no-show? (memq 'deleted style)]
               [extra-gtks (if (eq? client-gtk gtk)
                               null
                               (if hscroll-adj
                                   (list client-gtk hscroll-adj vscroll-adj)
                                   (list client-gtk)))])
    
    (set-size x y w h)

    (define dc (new dc% 
                    [gtk client-gtk]
                    [get-client-size (lambda ()
                                       (let ([w (box 0)]
                                             [h (box 0)])
                                         (get-client-size w h)
                                         (values (unbox w) (unbox h))))]
		    [window-lock (send (get-top-win) get-dc-lock)]
                    [get-window (lambda (client-gtk)
                                  (if is-combo?
                                      (get-subwindow client-gtk)
                                      (g_object_get_window client-gtk)))]))

    (gtk_widget_realize gtk)
    (gtk_widget_realize client-gtk)

    (when resize-box 
      (let ([r (make-GtkRequisition 0 0)])
        (gtk_widget_size_request hscroll-gtk r)
        (gtk_widget_set_size_request resize-box 
                                     (GtkRequisition-height r)
                                     (GtkRequisition-height r))))

    (g_signal_connect client-gtk "expose-event" handle_expose)
    (connect-key-and-mouse client-gtk)
    (connect-focus client-gtk)
    (gtk_widget_add_events client-gtk (bitwise-ior GDK_KEY_PRESS_MASK
                                                   GDK_BUTTON_PRESS_MASK
                                                   GDK_BUTTON_RELEASE_MASK
                                                   GDK_POINTER_MOTION_MASK
                                                   GDK_FOCUS_CHANGE_MASK
                                                   GDK_ENTER_NOTIFY_MASK
                                                   GDK_LEAVE_NOTIFY_MASK))
    (set-gtk-object-flags! client-gtk (bitwise-ior (get-gtk-object-flags client-gtk)
                                                   GTK_CAN_FOCUS))

    (when hscroll-adj
      (g_signal_connect hscroll-adj "value-changed" handle_value_changed_h))
    (when vscroll-adj
      (g_signal_connect vscroll-adj "value-changed" handle_value_changed_v))

    (define/override (direct-update?) #f)

    (define/public (get-dc) dc)

    (define/override (get-client-gtk) client-gtk)
    (define/override (handles-events?) #t)

    ;; For the moment, the client area always starts at the 
    ;; control area's top left
    (define/override (get-client-delta)
      (values 0 0))

    ;; Avoid multiple queued paints:
    (define paint-queued? #f)
    ;; To handle paint requests that happen while on-paint
    ;;  is being called already. kProbably doesn't happen,
    ;;  because expose callabcks should be in the right
    ;;  eventspace.
    (define now-drawing? #f)
    (define refresh-after-drawing? #f)

    (define/public (queue-paint)
      ;; can be called from any thread, including the event-pump thread
      (unless paint-queued?
        (set! paint-queued? #t)
        (queue-window-event this (lambda () 
                                   (set! paint-queued? #f)
                                   (set! now-drawing? #t)
                                   (on-paint)
                                   (set! now-drawing? #f)
                                   (when refresh-after-drawing?
                                     (set! refresh-after-drawing? #f)
                                     (refresh))))))

    (define/public (on-paint) (void))

    (define/override (refresh)
      (gtk_widget_queue_draw client-gtk))
    
    (define/public (reset-child-dcs)
      (when (dc . is-a? . dc%)
        (send dc reset-dc)))
    (define/override (maybe-register-as-child parent on?)
      (register-as-child parent on?)
      (when on? (reset-child-dcs)))

    (define/override (internal-on-client-size w h)
      (send dc reset-dc))
    (define/override (on-client-size w h) 
      (let ([xb (box 0)]
            [yb (box 0)])
        (get-size xb yb)
        (on-size (unbox xb) (unbox yb))))

    (define/public (show-scrollbars h? v?)
      (when hscroll-gtk
        (if h?
            (gtk_widget_show hscroll-gtk)
            (gtk_widget_hide hscroll-gtk)))
      (when vscroll-gtk
        (if v?
            (gtk_widget_show vscroll-gtk)
            (gtk_widget_hide vscroll-gtk))))

    (define/public (set-scrollbars h-step v-step
                                   h-len v-len
                                   h-page v-page
                                   h-pos v-pos
                                   auto?)
      (when hscroll-adj
        (gtk_adjustment_configure hscroll-adj h-pos 0 h-len 1 h-page h-page))
      (when vscroll-adj
        (gtk_adjustment_configure vscroll-adj v-pos 0 v-len 1 v-page v-page)))

    (define/private (dispatch which proc)
      (if (eq? which 'vertical)
          (when vscroll-adj (proc vscroll-adj))
          (when hscroll-adj (proc hscroll-adj))))

    (define/public (set-scroll-page which v)
      (dispatch which (lambda (adj)
                        (let ([old (gtk_adjustment_get_page_size adj)])
                          (unless (= old v)
                            (gtk_adjustment_set_page_size adj v)
                            (gtk_adjustment_set_page_increment adj v)
                            (gtk_adjustment_set_upper adj (+ (- v old)
                                                             (gtk_adjustment_get_upper adj))))))))
    (define/public (set-scroll-range which v)
      (dispatch which (lambda (adj)
                        (gtk_adjustment_set_upper adj (+ v (gtk_adjustment_get_page_size adj))))))
    (define/public (set-scroll-pos which v)
      (dispatch which (lambda (adj) (gtk_adjustment_set_value adj v))))

    (define/public (get-scroll-page which) 
      (->long (dispatch which (lambda (adj)
                                (- (gtk_adjustment_get_page_size adj)
                                   (gtk_adjustment_get_page_size adj))))))
    (define/public (get-scroll-range which)
      (->long (dispatch which gtk_adjustment_get_upper)))
    (define/public (get-scroll-pos which)
      (->long (dispatch which gtk_adjustment_get_value)))
    
    (define clear-bg?
      (and (not (memq 'transparent style)) 
           (not (memq 'no-autoclear style))))
    (define transparent?
      (memq 'transparent style))
    (define gc #f)
    (define bg-col (make-object color% "white"))
    (define/public (get-canvas-background) (if transparent?
                                               #f
                                               bg-col))
    (define/public (set-canvas-background col) (set! bg-col col))
    (define/public (get-canvas-background-for-clearing) 
      (if now-drawing?
          (begin
            (set! refresh-after-drawing? #t)
            #f)
          (if clear-bg?
              (let ([conv (lambda (x) (bitwise-ior x (arithmetic-shift x 8)))])
                (unless gc
                  (let ([w (g_object_get_window gtk)])
                    (set! gc (gdk_gc_new w))))
                (gdk_gc_set_rgb_fg_color gc (make-GdkColor 0
                                                           (conv (color-red bg-col))
                                                           (conv (color-green bg-col))
                                                           (conv (color-blue bg-col))))
                gc)
              #f)))

    (when is-combo?
      (connect-changed client-gtk))

    (define/public (append-combo-item str)
      (gtk_combo_box_append_text gtk str))

    (define/public (combo-maybe-clicked)
      (let ([i (gtk_combo_box_get_active gtk)])
        (when (i . > . -1)
          (gtk_combo_box_set_active gtk -1)
          (queue-window-event this (lambda () (on-combo-select i))))))
    (define/public (on-combo-select i) (void))

    (define/public (set-combo-text t) (void))

    (def/public-unimplemented set-background-to-gray)

    (define/public (do-scroll direction)
      (on-scroll (new scroll-event%
                      [event-type 'thumb]
                      [direction direction]
                      [position (get-scroll-pos direction)])))
    (define/public (on-scroll e) (void))

    (def/public-unimplemented scroll)
    (def/public-unimplemented warp-pointer)
    (def/public-unimplemented view-start)
    (define/public (set-resize-corner on?) (void))
    
    (def/public-unimplemented get-virtual-size)))
