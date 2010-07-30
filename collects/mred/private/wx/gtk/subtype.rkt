#lang racket/base
(require ffi/unsafe
         ffi/unsafe/alloc
         "utils.rkt"
         "const.rkt"
         "types.rkt")

(provide make-subclass
         GtkWidgetClass-expose_event
         set-GtkWidgetClass-expose_event!)

(define _GTypeClass _GType)

(define-cstruct _GObjectClass ([g_type_class _GTypeClass]
                               [construct_properties _pointer]
                               [constructor _fpointer]
                               [set_property _fpointer]
                               [get_property _fpointer]
                               [dispose _fpointer]
                               [finalize _fpointer]
                               [dispatch_properties _fpointer]
                               [notify _fpointer]
                               [constructed _fpointer]
                               [pdummy1 _pointer]
                               [pdummy2 _pointer]
                               [pdummy3 _pointer]
                               [pdummy4 _pointer]
                               [pdummy5 _pointer]
                               [pdummy6 _pointer]
                               [pdummy7 _pointer]))

(define-cstruct _GtkObjectClass ([parent_class _GObjectClass]
                                 [set_arg _fpointer]
                                 [get_arg _fpointer]
                                 [destroy _fpointer]))

(define-cstruct _GtkWidgetClass ([parent_class _GtkObjectClass]
                                 [activate_signal _uint]
                                 [set_scroll_adjustments_signal _uint]
                                 [dispatch_child_properties_changed _fpointer]
                                 [show _fpointer]
                                 [show_all _fpointer]
                                 [hide _fpointer]
                                 [hide_all _fpointer]
                                 [map _fpointer]
                                 [unmap _fpointer]
                                 [realize _fpointer]
                                 [unrealize _fpointer]
                                 [size_request _fpointer]
                                 [size_allocate _fpointer]
                                 [parent_set _fpointer]
                                 [hierarchy_changed _fpointer]
                                 [style_set _fpointer]
                                 [direction_changed _fpointer]
                                 [grab_notify _fpointer]
                                 [child_notify _fpointer]
                                 [mnemonic_activate _fpointer]
                                 [grab_focus _fpointer]
                                 [focus _fpointer]
                                 [event _fpointer]
                                 [button_press_event _fpointer]
                                 [button_release_event _fpointer]
                                 [scroll_event _fpointer]
                                 [motion_notify_event _fpointer]
                                 [delete_event _fpointer]
                                 [destroy_event _fpointer]
                                 [whatever _pointer] ;;; HACK!!!!!! Something is wrong so that expose shows up in the wrong place
                                 [expose_event (_fun #:atomic? #t _GtkWidget _GdkEventExpose -> _void)]
                                 [key_press_event _fpointer]
                                 [key_release_event _fpointer]
                                 [enter_notify_event _fpointer]
                                 [leave_notify_event _fpointer]
                                 [configure_event _fpointer]
                                 [focus_in_event _fpointer]
                                 [focus_out_event _fpointer]
                                 [map_event _fpointer]
                                 [unmap_event _fpointer]
                                 [property_notify_event _fpointer]
                                 [selection_clear_event _fpointer]
                                 [selection_request_event _fpointer]
                                 [selection_notify_event _fpointer]
                                 [proximity_in_event _fpointer]
                                 [proximity_out_event _fpointer]
                                 [visibility_notify_event _fpointer]
                                 [client_event _fpointer]
                                 [no_expose_event _fpointer]
                                 [window_state_event _fpointer]
                                 [selection_get _fpointer]
                                 [selection_received _fpointer]
                                 [drag_begin _fpointer]
                                 [drag_end _fpointer]
                                 [drag_data_get _fpointer]
                                 [drag_data_delete _fpointer]
                                 [drag_leave _fpointer]
                                 [drag_motion _fpointer]
                                 [drag_drop _fpointer]
                                 [drag_data_received _fpointer]
                                 [popup_menu _fpointer]
                                 [show_help _fpointer]
                                 [get_accessible _fpointer]
                                 [screen_changed _fpointer]
                                 [can_activate_accel _fpointer]
                                 [grab_broken_event _fpointer]
                                 [composited_changed _fpointer]
                                 [query_tooltip _fpointer]
                                 [gtk_reserved5 _fpointer]
                                 [gtk_reserved6 _fpointer]
                                 [gtk_reserved7 _fpointer]))

(define-cstruct _GTypeQuery ([type _GType]
                             [type_name _string]
                             [class_size _uint]
                             [instance_size _uint]))

(define-gobj g_type_query (_fun _GType _GTypeQuery-pointer -> _void))

(define-cstruct _GTypeInfo ([class_size _uint16]
                            [base_init _fpointer]
                            [base_finalize _fpointer]
                            [class_init (_fun #:atomic? #t _GtkWidgetClass-pointer _pointer -> _void)]
                            [class_finalize _fpointer]
                            [class_data _pointer]
                            [instance_size _uint16]
                            [n_preallocs _uint16]
                            [instance_init _fpointer]
                            [value_table _pointer]))

(define-gobj g_type_register_static (_fun _GType _string _GTypeInfo-pointer _int -> _GType))

(define saves null)

(define (make-subclass base-type name class-init-func)
  (when class-init-func
    (set! saves (cons class-init-func saves)))
  (let ([q (make-GTypeQuery 0 #f 0 0)])
    (g_type_query base-type q)
    (let ([ti (make-GTypeInfo (GTypeQuery-class_size q)
                              #f
                              #f
                              class-init-func
                              #f
                              #f
                              (GTypeQuery-instance_size q)
                              0
                              #f
                              #f)])
      (g_type_register_static base-type name ti 0))))

