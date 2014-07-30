#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides a base type environment including
;; most GUI library bindings

(require racket/require
         (subtract-in racket/gui/base
                      racket/draw
                      (except-in racket/snip get-the-snip-class-list))
         (for-syntax (only-in (rep type-rep)
                              make-Instance
                              make-Opaque))
         "draw.rkt"
         "snip.rkt"
         "private/gui-types.rkt"
         (for-syntax (submod "private/gui-types.rkt" #%type-decl)))

(provide (all-from-out "draw.rkt")
         (all-from-out "snip.rkt")
         (all-from-out "private/gui-types.rkt"))

(begin-for-syntax
 (define -Eventspace (make-Opaque #'eventspace?))
 (define -Color% (parse-type #'Color%))
 (define -Color%-Obj (make-Instance -Color%)))

(type-environment
 [button% (parse-type #'Button%)]
 [canvas% (parse-type #'Canvas%)]
 [check-box% (parse-type #'Check-Box%)]
 [checkable-menu-item% (parse-type #'Checkable-Menu-Item%)]
 [choice% (parse-type #'Choice%)]
 [clipboard-client% (parse-type #'Clipboard-Client%)]
 [combo-field% (parse-type #'Combo-Field%)]
 [column-control-event% (parse-type #'Column-Control-Event%)]
 [control-event% (parse-type #'Control-Event%)]
 [cursor% (parse-type #'Cursor%)]
 [dialog% (parse-type #'Dialog%)]
 [event% (parse-type #'Event%)]
 [frame% (parse-type #'Frame%)]
 [gauge% (parse-type #'Gauge%)]
 [group-box-panel% (parse-type #'Group-Box-Panel%)]
 [grow-box-spacer-pane% (parse-type #'Grow-Box-Spacer-Pane%)]
 [horizontal-pane% (parse-type #'Horizontal-Pane%)]
 [horizontal-panel% (parse-type #'Horizontal-Panel%)]
 [key-event% (parse-type #'Key-Event%)]
 [list-box% (parse-type #'List-Box%)]
 [menu% (parse-type #'Menu%)]
 [menu-bar% (parse-type #'Menu-Bar%)]
 [menu-item% (parse-type #'Menu-Item%)]
 [message% (parse-type #'Message%)]
 [mouse-event% (parse-type #'Mouse-Event%)]
 [pane% (parse-type #'Pane%)]
 [panel% (parse-type #'Panel%)]
 [popup-menu% (parse-type #'Popup-Menu%)]
 [printer-dc% (parse-type #'Printer-DC%)]
 [radio-box% (parse-type #'Radio-Box%)]
 [separator-menu-item% (parse-type #'Separator-Menu-Item%)]
 [scroll-event% (parse-type #'Scroll-Event%)]
 [slider% (parse-type #'Slider%)]
 [tab-panel% (parse-type #'Tab-Panel%)]
 [text-field% (parse-type #'Text-Field%)]
 [timer% (parse-type #'Timer%)]
 [vertical-pane% (parse-type #'Vertical-Pane%)]
 [vertical-panel% (parse-type #'Vertical-Panel%)]
 [the-font-list (make-Instance (parse-type #'Font-List%))]
 [get-face-list
  (->optkey [(one-of/c 'mono 'all)]
            #:all-variants? Univ #f
            (-lst -String))]
 [editor-canvas% (parse-type #'Editor-Canvas%)]
 [message-box (-> -String -String (one-of/c 'ok 'cancel 'yes 'no))]
 [open-input-text-editor
  (->optkey (make-Instance (parse-type #'Text%))
            [-Integer
             (Un (-val 'end) -Integer)
             (-> (make-Instance (parse-type #'Snip%))
                 (make-Instance (parse-type #'Snip%)))
             (make-Instance (parse-type #'Text%))
             -Boolean]
            #:lock-while-reading? Univ #f
            -Input-Port)]
 ;; Editor classes
 [editor-admin% (parse-type #'Editor-Admin%)]
 [editor-canvas% (parse-type #'Editor-Canvas%)]
 [editor-data% (parse-type #'Editor-Data%)]
 [editor-data-class% (parse-type #'Editor-Data-Class%)]
 [editor-stream-in% (parse-type #'Editor-Stream-In%)]
 [editor-stream-in-base% (parse-type #'Editor-Stream-In-Base%)]
 [editor-stream-out% (parse-type #'Editor-Stream-Out%)]
 [editor-stream-out-base% (parse-type #'Editor-Stream-Out-Base%)]
 [keymap% (parse-type #'Keymap%)]
 [pasteboard% (parse-type #'Pasteboard%)]
 [text% (parse-type #'Text%)]
 ;; 4.1 Dialogs
 [get-file
  (->optkey [(Un (-val #f) -String)
             (Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (Un (-val #f) -Pathlike)
             (Un (-val #f) -Pathlike)
             (Un (-val #f) -String)
             (-lst (one-of/c 'packages 'enter-packages 'common))
             (-lst (-lst* -String -String))]
            #:dialog-mixin (Un) #f
            (Un (-val #f) -Path))]
 [get-file-list
  (->optkey [(Un (-val #f) -String)
             (Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (Un (-val #f) -Pathlike)
             (Un (-val #f) -Pathlike)
             (Un (-val #f) -String)
             (-lst (one-of/c 'packages 'enter-packages 'common))
             (-lst (-lst* -String -String))]
            #:dialog-mixin (Un) #f
            (Un (-val #f) (-lst -Path)))]
 [put-file
  (->optkey [(Un (-val #f) -String)
             (Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (Un (-val #f) -Pathlike)
             (Un (-val #f) -Pathlike)
             (Un (-val #f) -String)
             (-lst (one-of/c 'packages 'enter-packages 'common))
             (-lst (-lst* -String -String))]
            #:dialog-mixin (Un) #f
            (Un (-val #f) -Path))]
 [get-directory
  (->optkey [(Un (-val #f) -String)
             (Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (Un (-val #f) -Pathlike)
             (-lst (one-of/c 'enter-packages 'common))]
            ;; FIXME: better type for this argument
            #:dialog-mixin (Un) #f
            (Un (-val #f) -Path))]
 [message-box
  (->optkey -String -String
            [(Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (-lst (one-of/c 'ok 'ok-cancel 'yes-no
                             'caution 'stop 'no-icon))]
            #:dialog-mixin (Un) #f
            (one-of/c 'ok 'cancel 'yes 'no))]
 [message-box/custom
  (->optkey -String -String
            (Un -String (make-Instance (parse-type #'Bitmap%)) (-val #f))
            (Un -String (make-Instance (parse-type #'Bitmap%)) (-val #f))
            (Un -String (make-Instance (parse-type #'Bitmap%)) (-val #f))
            [(Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (-lst (one-of/c 'stop 'caution 'no-icon 'number-order
                             'disallow-close 'no-default
                             'default=1 'default=2 'default=3))
             Univ]
            #:dialog-mixin (Un) #f
            Univ)]
 [message+check-box
  (->optkey -String -String -String
            [(Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (-lst (one-of/c 'ok 'ok-cancel 'yes-no
                             'caution 'stop 'no-icon 'checked))]
            #:dialog-mixin (Un) #f
            (-values (list (one-of/c 'ok 'cancel 'yes 'no)
                           -Boolean)))]
 [message+check-box
  (->optkey -String -String -String
            [(Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (-lst (one-of/c 'ok 'ok-cancel 'yes-no
                             'caution 'stop 'no-icon 'checked))]
            #:dialog-mixin (Un) #f
            (-values (list (one-of/c 'ok 'cancel 'yes 'no)
                           -Boolean)))]
 [message+check-box/custom
  (->optkey -String -String -String
            (Un -String (make-Instance (parse-type #'Bitmap%)) (-val #f))
            (Un -String (make-Instance (parse-type #'Bitmap%)) (-val #f))
            (Un -String (make-Instance (parse-type #'Bitmap%)) (-val #f))
            [(Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (-lst (one-of/c 'stop 'caution 'no-icon 'number-order
                             'disallow-close 'no-default
                             'default=1 'default=2 'default=3))
             Univ]
            #:dialog-mixin (Un) #f
            Univ)]
 [get-text-from-user
  (->optkey -String -String
            [(Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (-lst (one-of/c 'password 'disallow-invalid))]
            #:validate (-> -String -Boolean) #f
            #:dialog-mixin (Un) #f
            (Un (-val #f) -String))]
 [get-choices-from-user
  (->optkey -String -String (-lst -String)
            [(Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (-lst -Integer)
             (-lst (one-of/c 'single 'multiple 'extended))]
            (Un (-val #f) (-lst -Nat)))]
 [get-choices-from-user
  (->optkey [(Un (-val #f) -String)
             (Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (Un -Color%-Obj
                 (-val #f))
             (-lst (-val 'alpha))]
            (Un (-val #f) -Color%-Obj))]
 [get-font-from-user
  (->optkey [(Un (-val #f) -String)
             (Un (-val #f)
                 (make-Instance (parse-type #'Frame%))
                 (make-Instance (parse-type #'Dialog%)))
             (Un (make-Instance (parse-type #'Font%)) (-val #f))
             -Null]
            (Un (-val #f) (make-Instance (parse-type #'Font%))))]
 [can-get-page-setup-from-user? (-> -Boolean)]
 ;; 4.2 Eventspaces
 [#:opaque Eventspace eventspace?]
 [make-eventspace (-> -Eventspace)]
 [current-eventspace (-Param -Eventspace -Eventspace)]
 [event-dispatch-handler (-Param (-> -Eventspace Univ) (-> -Eventspace Univ))]
 [eventspace-event-evt
  (cl->* (-> (-evt -Eventspace))
         (-> -Eventspace (-evt -Eventspace)))]
 [eventspace-shutdown? (-> -Eventspace -Boolean)]
 [eventspace-handler-thread (-> -Eventspace (-opt -Thread))]
 [check-for-break (-> -Boolean)]
 [get-top-level-windows
  (-> (-lst (Un (make-Instance (parse-type #'Frame%))
                (make-Instance (parse-type #'Dialog%)))))]
 [get-top-level-focus-window
  (-> (Un (-val #f)
          (make-Instance (parse-type #'Frame%))
          (make-Instance (parse-type #'Dialog%))))]
 [get-top-level-edit-target-window
  (-> (Un (-val #f)
          (make-Instance (parse-type #'Frame%))
          (make-Instance (parse-type #'Dialog%))))]
 [special-control-key
  (cl->* (-> Univ -Void) (-> -Boolean))]
 [special-option-key
  (cl->* (-> Univ -Void) (-> -Boolean))]
 [queue-callback (->opt (-> Univ) [Univ] -Void)]
 [yield
  (-poly (a)
    (cl->* (-> -Boolean)
           (-> (-val 'wait) (-val #t))
           (-> (-evt a) a)))]
 [sleep/yield (-> -NonNegReal -Void)]
 ;; 4.4 Global Graphics
 [flush-display (-> -Void)]
 [get-display-count (-> -PosInt)]
 [get-display-depth (-> -Nat)]
 [get-display-left-top-inset
  (cl->* (->key #:monitor -Nat #f
                (-values (list (Un (-val #f) -Nat)
                               (Un (-val #f) -Nat))))
         (->key Univ #:monitor -Nat #f
                (-values (list (Un (-val #f) -Nat)
                               (Un (-val #f) -Nat)))))]
 [get-display-size
  (cl->* (->key #:monitor -Nat #f
                (-values (list (Un (-val #f) -Nat)
                               (Un (-val #f) -Nat))))
         (->key Univ #:monitor -Nat #f
                (-values (list (Un (-val #f) -Nat)
                               (Un (-val #f) -Nat)))))]
 [is-color-display? (-> -Boolean)]
 ;; 4.5 Fonts
 [menu-control-font (make-Instance (parse-type #'Font%))]
 [normal-control-font (make-Instance (parse-type #'Font%))]
 [small-control-font (make-Instance (parse-type #'Font%))]
 [tiny-control-font (make-Instance (parse-type #'Font%))]
 [view-control-font (make-Instance (parse-type #'Font%))]
 ;; 4.6 Miscellaneous
 [begin-busy-cursor (-> -Void)]
 [bell (-> -Void)]
 [dimension-integer? (-> Univ -Boolean)]
 [end-busy-cursor (-> -Void)]
 [file-creator-and-type
  (cl->* (-> -Path (-values (list -Bytes -Bytes)))
         (-> -Path -Bytes -Bytes -Void))]
 [find-graphical-system-path
  (-> (one-of/c 'init-file 'x-display) (-opt -Path))]
 [get-default-shortcut-prefix
  (-> (Un (-lst* (-val 'ctl))
          (-lst* (-val 'cmd))
          (-lst (one-of/c 'alt 'cmd 'meta 'ctl 'shift 'option))))]
 [get-panel-background (-> -Color%-Obj)]
 [get-highlight-background-color (-> -Color%-Obj)]
 [get-highlight-text-color (-> (-opt -Color%-Obj))]
 ; get-window-text-extent
 ; graphical-read-eval-print-loop
 ; textual-read-eval-print-loop
 ; get-current-mouse-state
 [hide-cursor-until-moved (-> -Void)]
 [is-busy? (-> -Boolean)]
 [label->plain-label (-> -String -String)]
 ; make-gl-bitmap
 [make-gui-empty-namespace (-> -Namespace)]
 [make-gui-namespace (-> -Namespace)]
 ; make-screen-bitmap
 ; play-sound
 ; position-integer?
 ; positive-dimension-integer?
 ; register-collecting-blit
 ; unregister-collecting-blit
 ; send-message-to-window
 ; spacing-integer?
 [system-position-ok-before-cancel? (-> -Boolean)]
 ; the-clipboard
 ; the-x-selection-clipboard
 ; label-string?
 ; key-code-symbol?
 ;; 8 Editor functions
 [get-the-snip-class-list (-> (make-Instance (parse-type #'Snip-Class-List<%>)))])
