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

(define-gtk gtk_hbox_new (_fun _gboolean _int -> _GtkWidget))
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

(define (handle-expose gtk event)
  (let ([wx (gtk->wx gtk)])
    (let ([gc (send wx get-canvas-background-for-clearing)])
      (when gc
        (gdk_draw_rectangle (g_object_get_window gtk) gc #t
                            0 0 32000 32000)))
    (queue-window-event wx (lambda ()
                             (send wx on-paint))))
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


(define canvas%
  (class (client-size-mixin window%)
    (init parent
          x y w h
          style
          [ignored-name #f]
          [gl-config #f])

    (inherit get-gtk set-size get-client-size)

    (define client-gtk (gtk_drawing_area_new))
    (define-values (gtk hscroll-adj vscroll-adj hscroll-gtk vscroll-gtk resize-box)
      (if (or (memq 'hscroll style)
              (memq 'vscroll style))
          (let ([hadj (gtk_adjustment_new 0.0 0.0 1.0 1.0 1.0 1.0)]
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
              (values h hadj vadj h2 v2 resize-box)))
          (values client-gtk #f #f #f #f #f)))

    (super-new [parent parent]
               [gtk gtk]
               [client-gtk client-gtk]
               [no-show? (memq 'deleted style)]
               [extra-gtks (if (eq? client-gtk gtk)
                               null
                               (list client-gtk hscroll-adj vscroll-adj))])
    
    (set-size x y w h)

    (define dc (new dc% 
                    [gtk client-gtk]
                    [get-client-size (lambda ()
                                       (let ([w (box 0)]
                                             [h (box 0)])
                                         (get-client-size w h)
                                         (values (unbox w) (unbox h))))]))

    (gtk_widget_realize gtk)
    (gtk_widget_realize client-gtk)

    (when resize-box 
      (let ([r (make-GtkRequisition 0 0)])
        (gtk_widget_size_request hscroll-gtk r)
        (gtk_widget_set_size_request resize-box 
                                     (GtkRequisition-height r)
                                     (GtkRequisition-height r))))

    (g_signal_connect client-gtk "expose_event" handle_expose)
    (connect-key-and-mouse client-gtk)
    (connect-focus client-gtk)
    (gtk_widget_add_events client-gtk (bitwise-ior GDK_KEY_PRESS_MASK
                                                   GDK_BUTTON_PRESS_MASK
                                                   GDK_BUTTON_RELEASE_MASK
                                                   GDK_POINTER_MOTION_MASK
                                                   GDK_FOCUS_CHANGE_MASK))
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

    (define/public (on-paint) (void))

    (define/override (internal-on-client-size w h)
      (send dc reset-dc-size))

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
    (define gc #f)
    (define bg-col (make-object color% "white"))
    (define/public (get-canvas-background) bg-col)
    (define/public (set-canvas-background col) (set! bg-col col))
    (define/public (get-canvas-background-for-clearing) 
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
          #f))

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
