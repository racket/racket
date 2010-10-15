#lang racket/base
(require ffi/unsafe
         racket/class
         racket/draw
         ffi/unsafe/alloc
         racket/draw/color
         racket/draw/local
         "../common/backing-dc.rkt"
         "../common/canvas-mixin.rkt"
         "../../syntax.rkt"
	 "../../lock.rkt"
         "../common/event.rkt"
         "utils.rkt"
         "const.rkt"
         "types.rkt"
         "window.rkt"
         "client-window.rkt"
         "widget.rkt"
         "dc.rkt"
         "gl-context.rkt"
         "combo.rkt"
         "pixbuf.rkt"
         "gcwin.rkt")

(provide canvas%)

;; ----------------------------------------

(define-gobj g_object_freeze_notify (_fun _GtkWidget -> _void))
(define-gobj g_object_thaw_notify (_fun _GtkWidget -> _void))

(define-gobj g_object_set_double (_fun _GtkWidget _string _double* (_pointer = #f) -> _void)
  #:c-id g_object_set)
(define-gobj g_object_get_double (_fun _GtkWidget _string (r : (_ptr o _double)) (_pointer = #f) 
				       -> _void -> r)
  #:c-id g_object_get)

(define-gtk gtk_drawing_area_new (_fun -> _GtkWidget))

(define-gtk gtk_combo_box_entry_new_text (_fun -> _GtkWidget))
(define-gtk gtk_combo_box_append_text (_fun _GtkWidget _string -> _void))

(define-gtk gtk_widget_queue_draw (_fun _GtkWidget -> _void))

(define-gtk gtk_hscrollbar_new (_fun _pointer -> _GtkWidget))
(define-gtk gtk_vscrollbar_new (_fun _pointer -> _GtkWidget))

(define-gtk gtk_widget_set_double_buffered (_fun _GtkWidget _gboolean -> _void))

(define _GtkAdjustment _GtkWidget) ; no, actually a GtkObject
(define-gtk gtk_adjustment_new (_fun _double* _double* _double* _double* _double* _double* -> _GtkAdjustment))
(define-gtk gtk_adjustment_configure (_fun _GtkAdjustment _double* _double* _double* _double* _double* _double* -> _void)
  #:fail (lambda ()
	   ;; This by-hand version doesn't produce quite the same notifications.
	   (lambda (gtk value lower upper step-inc page-inc page-size)
	     (atomically
              (g_object_freeze_notify gtk)
              (g_object_set_double gtk "lower" lower)
              (g_object_set_double gtk "upper" upper)
              (g_object_set_double gtk "step-increment" step-inc)
              (g_object_set_double gtk "page-increment" page-inc)
              (g_object_set_double gtk "page-size" page-size)
              (let ([value (max lower (min value (- upper page-size)))])
                (gtk_adjustment_set_value gtk value))
              (g_object_thaw_notify gtk)))))
(define-gtk gtk_adjustment_get_value (_fun _GtkAdjustment -> _double*))
(define-gtk gtk_adjustment_set_value (_fun _GtkAdjustment _double* -> _void))
(define-gtk gtk_adjustment_get_upper (_fun _GtkAdjustment -> _double*)
  #:fail (lambda ()
	   (lambda (gtk)
	     (g_object_get_double gtk "upper"))))
(define-gtk gtk_adjustment_set_upper (_fun _GtkAdjustment _double* -> _void)
  #:fail (lambda ()
	   (lambda (gtk upper)
	     (g_object_set_double gtk "upper" upper))))
(define-gtk gtk_adjustment_get_page_size (_fun _GtkAdjustment -> _double*)
  #:fail (lambda ()
	   (lambda (gtk)
	     (g_object_get_double gtk "page-size"))))
(define-gtk gtk_adjustment_set_page_size (_fun _GtkAdjustment _double* -> _void)
  #:fail (lambda ()
	   (lambda (gtk page-size)
	     (g_object_set_double gtk "page-size" page-size))))
(define-gtk gtk_adjustment_get_page_increment (_fun _GtkAdjustment -> _double*)
  #:fail (lambda ()
	   (lambda (gtk)
	     (g_object_get_double gtk "page-increment"))))
(define-gtk gtk_adjustment_set_page_increment (_fun _GtkAdjustment _double* -> _void)
  #:fail (lambda ()
	   (lambda (gtk page-inc)
	     (g_object_set_double gtk "page-increment" page-inc))))

(define-gtk gtk_container_remove (_fun _GtkWidget _GtkWidget -> _void))
(define-gtk gtk_bin_get_child (_fun _GtkWidget -> _GtkWidget))

(define-gtk gtk_container_set_border_width (_fun _GtkWidget _int -> _void))

(define-gobj g_object_set_bool (_fun _GtkWidget _string _gboolean [_pointer = #f] -> _void)
  #:c-id g_object_set)

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
  (let* ([win (widget-window gtk)]
         [subs (gdk_window_get_children win)])
    (if subs
        (GList-data subs)
        win)))

(define-signal-handler connect-changed "changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (send wx combo-maybe-clicked)))))

(define-gtk gtk_combo_box_set_active (_fun _GtkWidget _int -> _void))
(define-gtk gtk_combo_box_get_active (_fun _GtkWidget -> _int))

(define-signal-handler connect-expose "expose-event"
  (_fun _GtkWidget _GdkEventExpose-pointer -> _gboolean)
  (lambda (gtk event)
    (let ([wx (gtk->wx gtk)])
      (when wx
        (unless (send wx paint-or-queue-paint)
          (let ([gc (send wx get-canvas-background-for-clearing)])      
            (when gc
              (gdk_draw_rectangle (widget-window gtk) gc #t
                                  0 0 32000 32000)
              (gdk_gc_unref gc))))))
    #t))

(define-signal-handler connect-expose-border "expose-event"
  (_fun _GtkWidget _GdkEventExpose-pointer -> _gboolean)
  (lambda (gtk event)
    (let* ([win (widget-window gtk)]
           [gc (gdk_gc_new win)]
           [gray #x8000])
      (when gc
        (gdk_gc_set_rgb_fg_color gc (make-GdkColor 0 gray gray gray))
        (let ([r (GdkEventExpose-area event)])
          (gdk_draw_rectangle win gc #t 
                              (GdkRectangle-x r)
                              (GdkRectangle-y r)
                              (GdkRectangle-width r)
                              (GdkRectangle-height r)))
        (gdk_gc_unref gc)))))

(define-signal-handler connect-value-changed-h "value-changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (do-value-changed gtk 'horizontal)))

(define-signal-handler connect-value-changed-v "value-changed"
  (_fun _GtkWidget -> _void)
  (lambda (gtk)
    (do-value-changed gtk 'vertical)))

(define (do-value-changed gtk dir)
  (let ([wx (gtk->wx gtk)])
    (when wx
      (when (send wx deliver-scroll-callbacks?)
        (queue-window-event wx (lambda () 
                                 (send wx do-scroll dir)
                                 (flush-display))))))
  #t)

(define canvas%
  (canvas-mixin
   (class (canvas-autoscroll-mixin (client-size-mixin window%))
     (init parent
           x y w h
           style
           [ignored-name #f]
           [gl-config #f])

     (inherit get-gtk set-size get-size get-client-size 
              on-size get-top-win
              set-auto-size 
              adjust-client-delta infer-client-delta
              is-auto-scroll? get-virtual-width get-virtual-height
              refresh-for-autoscroll)

     (define is-combo? (memq 'combo style))
     (define has-border? (or (memq 'border style)
                             (memq 'control-border style)))

     (define margin (if has-border? 1 0))

     (define-values (client-gtk gtk 
                                hscroll-adj vscroll-adj hscroll-gtk vscroll-gtk resize-box
                                combo-button-gtk
                                scroll-width)
       (atomically ;; need to connect all children to gtk to avoid leaks
        (cond
         [(or (memq 'hscroll style)
              (memq 'vscroll style))
          (let* ([client-gtk (gtk_drawing_area_new)]
                 [hadj (gtk_adjustment_new 0.0 0.0 1.0 1.0 1.0 1.0)]
                 [vadj (gtk_adjustment_new 0.0 0.0 1.0 1.0 1.0 1.0)])
            (let ([h (as-gtk-allocation (gtk_hbox_new #f 0))]
                  [v (gtk_vbox_new #f 0)]
                  [v2 (gtk_vbox_new #f 0)]
                  [h2 (gtk_vbox_new #f 0)]
                  [hscroll (gtk_hscrollbar_new hadj)]
                  [vscroll (gtk_vscrollbar_new vadj)]
                  [resize-box (gtk_drawing_area_new)])
              ;; |------------------------------------|
              ;; | h |-----------------| |-----------||
              ;; |   | v               | | v2        ||
              ;; |   |                 | | [vscroll] ||
              ;; |   | [h2 [hscroll]]  | | [resize]  ||
              ;; |   |-----------------| |-----------|| 
              ;; |------------------------------------|
              (when has-border?
                (gtk_container_set_border_width h margin))
              (gtk_box_pack_start h v #t #t 0)
              (gtk_box_pack_start v client-gtk #t #t 0)
              (gtk_box_pack_start h v2 #f #f 0)
              (gtk_box_pack_start v2 vscroll #t #t 0)
              (gtk_box_pack_start v h2 #f #f 0)
              (gtk_box_pack_start h2 hscroll #t #t 0)
              (gtk_box_pack_start v2 resize-box #f #f 0)
              (when (memq 'hscroll style)
                (gtk_widget_show hscroll))
              (gtk_widget_show vscroll)
              (gtk_widget_show h)
              (gtk_widget_show v)
              (when (memq 'vscroll style)
                (gtk_widget_show v2))
              (gtk_widget_show h2)
              (when (memq 'hscroll style)
                (gtk_widget_show resize-box))
              (gtk_widget_show client-gtk)
              (let ([req (make-GtkRequisition 0 0)])
                (gtk_widget_size_request vscroll req)
                (values client-gtk h hadj vadj 
                        (and (memq 'hscroll style) h2)
                        (and (memq 'vscroll style) v2)
                        (and (memq 'hscroll style) (memq 'vscroll style) resize-box)
                        #f
                        (GtkRequisition-width req)))))]
         [is-combo?
          (let* ([gtk (as-gtk-allocation (gtk_combo_box_entry_new_text))]
                 [orig-entry (gtk_bin_get_child gtk)])
            (values orig-entry gtk #f #f #f #f #f (extract-combo-button gtk) 0))]
         [has-border?
          (let ([client-gtk (gtk_drawing_area_new)]
                [h (as-gtk-allocation (gtk_hbox_new #f 0))])
            (gtk_box_pack_start h client-gtk #t #t 0)
            (gtk_container_set_border_width h margin)
            (connect-expose-border h)
            (gtk_widget_show client-gtk)
            (values client-gtk h #f #f #f #f #f #f 0))]
         [else
          (let ([client-gtk (as-gtk-allocation (gtk_drawing_area_new))])
            (values client-gtk client-gtk #f #f #f #f #f #f 0))])))

     (super-new [parent parent]
                [gtk gtk]
                [client-gtk client-gtk]
                [no-show? (memq 'deleted style)]
                [extra-gtks (if (eq? client-gtk gtk)
                                null
                                (if hscroll-adj
                                    (list client-gtk hscroll-adj vscroll-adj)
                                    (if combo-button-gtk
                                        (list client-gtk combo-button-gtk)
                                        (list client-gtk))))])

     (set-size x y w h)
     
     (define dc (new dc% [canvas this]))

     (define for-gl? (memq 'gl style))
     (when for-gl?
       (prepare-widget-gl-context client-gtk gl-config))

     (gtk_widget_realize gtk)
     (gtk_widget_realize client-gtk)

     (when resize-box 
       (let ([r (make-GtkRequisition 0 0)])
         (gtk_widget_size_request hscroll-gtk r)
         (gtk_widget_set_size_request resize-box 
                                      (GtkRequisition-height r)
                                      (GtkRequisition-height r))))

     (connect-expose client-gtk)
     #;(gtk_widget_set_double_buffered client-gtk #f)
     (connect-key-and-mouse client-gtk)
     (connect-focus client-gtk)
     (gtk_widget_add_events client-gtk (bitwise-ior GDK_KEY_PRESS_MASK
                                                    GDK_KEY_RELEASE_MASK
                                                    GDK_BUTTON_PRESS_MASK
                                                    GDK_BUTTON_RELEASE_MASK
                                                    GDK_POINTER_MOTION_MASK
                                                    GDK_FOCUS_CHANGE_MASK
                                                    GDK_ENTER_NOTIFY_MASK
                                                    GDK_LEAVE_NOTIFY_MASK))
     (unless (memq 'no-focus style)
       (set-gtk-object-flags! client-gtk (bitwise-ior (get-gtk-object-flags client-gtk)
                                                      GTK_CAN_FOCUS)))
     (when combo-button-gtk
       (connect-combo-key-and-mouse combo-button-gtk))

     (when hscroll-adj (connect-value-changed-h hscroll-adj))
     (when vscroll-adj (connect-value-changed-v vscroll-adj))

     (set-auto-size)
     (adjust-client-delta (+ (* 2 margin) 
                             (if (memq 'vscroll style)
                                 scroll-width
                                 0))
                          (+ (* 2 margin) 
                             (if (memq 'hscroll style)
                                 scroll-width
                                 0)))

     (define/override (direct-update?) #f)

     (define/public (get-dc) dc)

     (define/public (make-compatible-bitmap w h)
       (send dc make-backing-bitmap w h))

     (define/override (get-client-gtk) client-gtk)
     (define/override (handles-events? gtk) (not (ptr-equal? gtk combo-button-gtk)))

     (define/override (get-client-delta)
       (values margin margin))

     ;; The `queue-paint' and `paint-children' methods
     ;; are defined by `canvas-mixin' from ../common/canvas-mixin
     (define/public (queue-paint) (void))
     (define/public (request-canvas-flush-delay)
       (request-flush-delay client-gtk))
     (define/public (cancel-canvas-flush-delay req)
       (cancel-flush-delay req))
     (define/public (queue-canvas-refresh-event thunk)
       (queue-window-refresh-event this thunk))

     (define/public (paint-or-queue-paint)
       (if for-gl?
	   (queue-paint)
	   (or (do-backing-flush this dc (if is-combo?
					     (get-subwindow client-gtk)
					     (widget-window client-gtk)))
	       (begin
		 (queue-paint)
		 #f))))

     (define/public (on-paint) (void))

     (define/public (get-flush-window) client-gtk)

     (define/public (begin-refresh-sequence)
       (send dc suspend-flush))
     (define/public (end-refresh-sequence)
       (send dc resume-flush))

     (define/override (refresh)
       (queue-paint))

     (define/public (queue-backing-flush)
       ;; called atomically (not expecting exceptions)
       (unless for-gl?
         (gtk_widget_queue_draw client-gtk)))
     
     (define/override (reset-child-dcs)
       (when (dc . is-a? . dc%)
         (reset-dc)))

     (send dc start-backing-retained)

     (define/private (reset-dc)
       (send dc reset-backing-retained)
       (refresh)
       (send dc set-auto-scroll
             (if (get-virtual-width)
                 (gtk_adjustment_get_value hscroll-adj)
                 0)
             (if (get-virtual-height)
                 (gtk_adjustment_get_value vscroll-adj)
                 0)))

     (define/override (internal-on-client-size w h)
       (reset-dc))
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
             (gtk_widget_hide vscroll-gtk)))
       (when (and hscroll-gtk vscroll-gtk)
         (cond
          [(and v? h?)
           (gtk_widget_show resize-box)]
          [(and v? (not h?))
           ;; remove corner 
           (gtk_widget_hide resize-box)]))
       (adjust-client-delta (+ (* 2 margin) (if v? scroll-width 0))
                            (+ (* 2 margin) (if h? scroll-width 0))))

     (define suspend-scroll-callbacks? #f)
     (define/public (deliver-scroll-callbacks?) (not suspend-scroll-callbacks?))
     (define/private (as-scroll-change thunk)
       (atomically
        (set! suspend-scroll-callbacks? #t)
        (begin0
         (thunk)
         (set! suspend-scroll-callbacks? #f))))


     (define/private (configure-adj adj scroll-gtk len page pos)
       (when (and scroll-gtk adj)
         (as-scroll-change
          (lambda ()
            (if (zero? len)
                (gtk_adjustment_configure adj 0 0 1 1 1 1)
                (gtk_adjustment_configure adj pos 0 (+ len page) 1 page page))))))

     (define/override (do-set-scrollbars h-step v-step
                                         h-len v-len
                                         h-page v-page
                                         h-pos v-pos)
       (configure-adj hscroll-adj hscroll-gtk h-len h-page h-pos)
       (configure-adj vscroll-adj vscroll-gtk v-len v-page v-pos))

     (define/override (reset-dc-for-autoscroll)
       (reset-dc))

     (define/private (dispatch which proc [default (void)])
       (if (eq? which 'vertical)
           (if vscroll-adj (proc vscroll-adj) default)
           (if hscroll-adj (proc hscroll-adj) default)))

     (define/public (set-scroll-page which v)
       (dispatch which (lambda (adj)
                         (let ([old (gtk_adjustment_get_page_size adj)])
                           (unless (= old v)
                             (as-scroll-change
                              (lambda ()
                                (gtk_adjustment_set_page_size adj v)
                                (gtk_adjustment_set_page_increment adj v)
                                (gtk_adjustment_set_upper adj (+ (- v old)
                                                                 (gtk_adjustment_get_upper adj))))))))))
     (define/public (set-scroll-range which v)
       (dispatch which (lambda (adj)
                         (as-scroll-change
                          (lambda ()
                            (gtk_adjustment_set_upper adj (+ v (gtk_adjustment_get_page_size adj))))))))
     (define/public (set-scroll-pos which v)
       (dispatch which (lambda (adj) 
                         (as-scroll-change
                          (lambda ()
                            (gtk_adjustment_set_value adj v))))))

     (define/public (get-scroll-page which) 
       (->long (dispatch which gtk_adjustment_get_page_size 0)))
     (define/public (get-scroll-range which)
       (->long (dispatch which (lambda (adj)
                                 (- (gtk_adjustment_get_upper adj)
                                    (gtk_adjustment_get_page_size adj)))
                         0)))
     (define/public (get-scroll-pos which)
       (->long (dispatch which gtk_adjustment_get_value 0)))
     
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
     (define/public (get-canvas-background-for-backing) (and clear-bg? bg-col))
     (define/public (get-canvas-background-for-clearing) 
       ;; called in event-dispatch mode
       (if clear-bg?
           (let* ([conv (lambda (x) (bitwise-ior x (arithmetic-shift x 8)))]
                  [w (widget-window gtk)]
                  [gc (gdk_gc_new w)])
             (gdk_gc_set_rgb_fg_color gc (make-GdkColor 0
                                                        (conv (color-red bg-col))
                                                        (conv (color-green bg-col))
                                                        (conv (color-blue bg-col))))
             gc)
           #f))

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
       (if (is-auto-scroll?)
           (refresh-for-autoscroll)
           (on-scroll (new scroll-event%
                           [event-type 'thumb]
                           [direction direction]
                           [position (get-scroll-pos direction)]))))
     (define/public (on-scroll e) (void))

     (define/public (scroll x y)
       (as-scroll-change
        (lambda ()
          (when hscroll-adj (gtk_adjustment_set_value hscroll-adj x))
          (when vscroll-adj (gtk_adjustment_set_value vscroll-adj y))))
       (when (is-auto-scroll?) (refresh-for-autoscroll)))

     (def/public-unimplemented warp-pointer)

     (define/override (get-virtual-h-pos)
      (gtk_adjustment_get_value hscroll-adj))
    (define/override (get-virtual-v-pos)
      (gtk_adjustment_get_value vscroll-adj))

     (define/public (set-resize-corner on?) (void))

     (define reg-blits null)
     
     (define/private (register-one-blit x y w h on-pixbuf off-pixbuf)
       (let* ([cwin (widget-window client-gtk)])
         (atomically
          (let ([win (create-gc-window cwin x y w h)])
            (let ([r (scheme_add_gc_callback
                      (make-gc-show-desc win on-pixbuf w h)
                      (make-gc-hide-desc win off-pixbuf w h))])
              (cons win r))))))
     
     (define/public (register-collecting-blit x y w h on off on-x on-y off-x off-y)
       (let ([on (fix-bitmap-size on w h on-x on-y)]
             [off (fix-bitmap-size off w h off-x off-y)])
         (let ([on-pixbuf (bitmap->pixbuf on)]
               [off-pixbuf (bitmap->pixbuf off)])
           (atomically
            (set! reg-blits (cons (register-one-blit x y w h on-pixbuf off-pixbuf) reg-blits))))))
     
     (define/public (unregister-collecting-blits)
       (atomically
        (for ([r (in-list reg-blits)])
          (g_object_unref (car r))
          (scheme_remove_gc_callback (cdr r)))
        (set! reg-blits null))))))
