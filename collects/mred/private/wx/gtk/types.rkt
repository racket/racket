#lang scheme/base
(require scheme/foreign)
(unsafe!)

(provide _GdkWindow
         _GtkWidget _GtkWindow
         _GdkDisplay
         _gpointer
         _GType

         _fnpointer
         _gboolean
         _gfloat

         _GdkEventButton _GdkEventButton-pointer
         (struct-out GdkEventButton)
         _GdkEventKey _GdkEventKey-pointer
         (struct-out GdkEventKey)
         _GdkEventScroll _GdkEventScroll-pointer
         (struct-out GdkEventScroll)
         _GdkEventMotion _GdkEventMotion-pointer
         (struct-out GdkEventMotion)
         _GdkEventCrossing _GdkEventCrossing-pointer
         (struct-out GdkEventCrossing)
         _GdkEventConfigure _GdkEventConfigure-pointer
         (struct-out GdkEventConfigure)
         _GdkEventExpose _GdkEventExpose-pointer
         (struct-out GdkEventExpose)
         (struct-out GdkRectangle))

(define _GType _long)

(define _GdkWindow (_cpointer/null 'GdkWindow))

(define _GtkWidget (_cpointer 'GtkWidget))
(define _GtkWindow _GtkWidget)

(define _GdkDisplay (_cpointer 'GdkDisplay))

(define _gpointer _GtkWidget)

(define _GdkDevice (_cpointer 'GdkDevice))

(define _fnpointer _pointer) ; a function pointer that can be NULL
(define _gboolean _bool)
(define _gfloat _float)
(define _GdkEventType _int)

(define-cstruct _GdkEventButton ([type _GdkEventType]
                                 [window _GdkWindow]
                                 [send_event _byte]
                                 [time _uint32]
                                 [x _double]
                                 [y _double]
                                 [axes _pointer] ; array of _double
                                 [state _uint]
                                 [button _uint]
                                 [device _GdkDevice]
                                 [x_root _double]
                                 [y_root _double]))


(define-cstruct _GdkEventKey ([type _GdkEventType]
                              [window _GdkWindow]
                              [send_event _byte]
                              [time _uint32]
                              [state _uint]
                              [keyval _uint]
                              [length _int]
                              [string _pointer] ; do not use
                              [hardware_keycode _uint16]
                              [group _ubyte]
                              [is_modifier _byte])) ; just 1 bit

(define-cstruct _GdkEventScroll ([type _GdkEventType]
                                 [window _GdkWindow]
                                 [send_event _byte]
                                 [time _uint32]
                                 [x _double]
                                 [y _double]
                                 [state _uint]
                                 [direction _uint]
                                 [device _GdkDevice]
                                 [x_root _double]
                                 [y_root _double]))

(define-cstruct _GdkEventMotion ([type _GdkEventType]
                                 [window _GdkWindow]
                                 [send_event _byte]
                                 [time _uint32]
                                 [x _double]
                                 [y _double]
                                 [axes _pointer]
                                 [state _uint]
                                 [is_hint _int16]
                                 [device _GdkDevice]
                                 [x_root _double]
                                 [y_root _double]))

(define-cstruct _GdkEventCrossing ([type _GdkEventType]
                                   [window _GdkWindow]
                                   [send_event _byte]
                                   [time _uint32]
                                   [x _double]
                                   [y _double]
                                   [x_root _double]
                                   [y_root _double]
                                   [mode _int]
                                   [detail _int]
                                   [focus _gboolean]
                                   [state _uint]))

(define-cstruct _GdkEventConfigure ([type _GdkEventType]
                                    [window _GdkWindow]
                                    [send_event _byte]
                                    [x _int]
                                    [y _int]
                                    [width _int]
                                    [height _int]))

(define-cstruct _GdkRectangle ([x _int]
                               [y _int]
                               [width _int]
                               [height _int]))

(define-cstruct _GdkEventExpose ([type _GdkEventType]
                                 [window _GdkWindow]
                                 [send_event _byte]
                                 [area _GdkRectangle]
                                 [region _pointer]
                                 [count _int]))
