#lang s-exp typed-racket/base-env/extra-env-lang

;; A typed wrapper for the framework library

(require framework
         framework/splash
         (for-syntax (only-in (rep type-rep)
                              make-Instance
                              make-HeterogeneousVector
                              [make-Opaque -opq]))
         "racket/private/gui-types.rkt"
         (for-syntax (submod "racket/private/gui-types.rkt" #%type-decl))
         "private/framework-types.rkt"
         (for-syntax (submod "private/framework-types.rkt" #%type-decl)))

(provide (all-from-out "private/framework-types.rkt"))

(begin-for-syntax
 (define -Button% (parse-type #'Button%))
 (define -Event% (parse-type #'Event%))
 (define -Color% (parse-type #'Color%))
 (define -Style-Delta% (parse-type #'Style-Delta%))
 (define -Area-Container<%> (parse-type #'Area-Container<%>))
 (define -Panel% (parse-type #'Panel%))
 (define -Vertical-Panel% (parse-type #'Vertical-Panel%))
 (define -Snip-Class% (parse-type #'Snip-Class%))
 (define -Keymap% (parse-type #'Keymap%))
 (define -Top-Level-Window<%> (parse-type #'Top-Level-Window<%>))
 (define -Dialog% (parse-type #'Dialog%))
 (define -Frame% (parse-type #'Frame%))
 (define -Menu% (parse-type #'Menu%))
 (define -Menu-Item% (parse-type #'Menu-Item%))
 (define -Bitmap% (parse-type #'Bitmap%))
 (define -Menu-Item-Container<%> (parse-type #'Menu-Item-Container<%>))
 (define -Window<%> (parse-type #'Window<%>))
 (define -Cursor% (parse-type #'Cursor%))
 (define -Popup-Menu% (parse-type #'Popup-Menu%))
 (define -Editor<%> (parse-type #'Editor<%>))
 (define -Snip% (parse-type #'Snip%))
 (define -Area-Container-Window<%> (parse-type #'Area-Container-Window<%>))
 (define -Text% (parse-type #'Text%))
 (define -Editor-Wordbreak-Map% (parse-type #'Editor-Wordbreak-Map%))
 (define -DC<%> (parse-type #'DC<%>))
 (define -Canvas% (parse-type #'Canvas%))
 (define -Key-Event% (parse-type #'Key-Event%))
 (define -Mouse-Event% (parse-type #'Mouse-Event%))
 (define -Radio-Box% (parse-type #'Radio-Box%))
 (define -Check-Box% (parse-type #'Check-Box%))
 (define -Choice% (parse-type #'Choice%))
 (define -List-Box% (parse-type #'List-Box%))
 (define -Area<%> (parse-type #'Area<%>))
 (define -Frame:Editor<%> (parse-type #'Frame:Editor<%>))

 (define -Color-Prefs:Color-Scheme-Style-Name (-opq #'color-prefs:color-scheme-style-name?))
 (define -Color-Prefs:Known-Color-Scheme-Name (-opq #'color-prefs:known-color-scheme-name?))
 (define -Color-Model:Xyz (-opq #'color-model:xyz?))
 (define -Eventspace (-opq #'eventspace?))
 (define -Handler:Handler (-opq #'handler:handler?))
 (define -Printable Univ) ; FIXME printable/c from 27
 (define -Exn:Unknown-Preference (-opq #'exn:unknown-preference?))
 (define -Preferences:Snapshot (-opq #'preferences:snapshot?))
 (define -Continuation-Mark-Set (-opq #'continuation-mark-set?))
 (define -Struct Univ) ; FIXME
 (define -Text:Range (-opq #'text:range?))

 ; Frequently reused instances
 (define -Color%-Instance (make-Instance -Color%))
 (define -Mouse-Event%-Instance (make-Instance -Mouse-Event%))
 (define -Key-Event%-Instance (make-Instance -Key-Event%))
 (define -Canvas%-Instance (make-Instance -Canvas%))
 (define -Vertical-Panel%-Instance (make-Instance -Vertical-Panel%))
 (define -Area-Container<%>-Instance (make-Instance -Area-Container<%>))
 (define -Area-Container-Window<%>-Instance (make-Instance -Area-Container-Window<%>))
 (define -Snip%-Instance (make-Instance -Snip%))
 (define -Editor<%>-Instance (make-Instance -Editor<%>))
 (define -Cursor%-Instance (make-Instance -Cursor%))
 (define -Menu%-Instance (make-Instance -Menu%))
 (define -Frame:Editor<%>-Instance (make-Instance -Frame:Editor<%>))
 (define -Button%-Instance (make-Instance -Button%))
 (define -Event%-Instance (make-Instance -Event%))
 (define -Bitmap%-Instance (make-Instance -Bitmap%))
 (define -Top-Level-Window<%>-Instance (make-Instance -Top-Level-Window<%>))
 (define -Dialog%-Instance (make-Instance -Dialog%))
 (define -Frame%-Instance (make-Instance -Frame%))
 (define -Style-Delta%-Instance (make-Instance -Style-Delta%))
 (define -Keymap%-Instance (make-Instance -Keymap%))
 (define -Area<%>-Instance (make-Instance -Area<%>))
 (define -Window<%>-Instance (make-Instance -Window<%>))
 (define -DC<%>-Instance (make-Instance -DC<%>)))

(type-environment
 [application:current-app-name (-Param -String)]
 ;; 3 Autosave
 [autosave:register
  ((make-Instance (parse-type #'Editor<%>-And-Autosave:Autosavable<%>)) . -> . -Void)]
 [autosave:toc-path -Path]
 [autosave:restore-autosave-files/gui (-> -Void)]
 ;; 4 Canvas
 [canvas:basic-mixin (parse-type #'Canvas:Basic-Mixin)]
 [canvas:color-mixin (parse-type #'Canvas:Color-Mixin)]
 [canvas:delegate-mixin (parse-type #'Canvas:Delegate-Mixin)]
 [canvas:info-mixin (parse-type #'Canvas:Info-Mixin)]
 [canvas:wide-snip-mixin (parse-type #'Canvas:Wide-Snip-Mixin)]
 [canvas:basic% (parse-type #'Canvas:Basic%)]
 [canvas:color% (parse-type #'Canvas:Color%)]
 [canvas:info% (parse-type #'Canvas:Info%)]
 [canvas:delegate% (parse-type #'Canvas:Delegate%)]
 [canvas:wide-snip% (parse-type #'Canvas:Wide-Snip%)]
 ;; 5 Color Model
 [color-model:rgb->xyz (-Nat -Nat -Nat . -> . -Color-Model:Xyz)]
 [color-model:rgb-color-distance
  (-Nat -Nat -Nat -Nat -Nat -Nat . -> . -NonNegReal)]
 [color-model:xyz->rgb
  (-NonNegReal -NonNegReal -NonNegReal . -> . (-Tuple (list -Nat -Nat -Nat)))]
 [#:opaque Color-Model:Xyz color-model:xyz?]
 [color-model:xyz-x (-Color-Model:Xyz . -> . -NonNegReal)]
 [color-model:xyz-y (-Color-Model:Xyz . -> . -NonNegReal)]
 [color-model:xyz-z (-Color-Model:Xyz . -> . -NonNegReal)]
 ;; 6 Color Prefs
 [color-prefs:set-default/color-scheme
  (-Symbol (Un -Color%-Instance -String) (Un -Color%-Instance -String)
           . -> . -Void)]
 [color-prefs:register-color-preference
  (->optkey
   -Symbol -String (Un -Color%-Instance -Style-Delta%-Instance)
   [(Un -String -Color%-Instance -False)]
   #:background (-opt -Color%-Instance) #f
   -Void)]
 [color-prefs:add-background-preferences-panel (-> -Void)]
 [color-prefs:add-to-preferences-panel
  (-String (-Vertical-Panel%-Instance . -> . -Void) . -> . -Void)]
 [color-prefs:build-color-selection-panel
  (->optkey
   -Area-Container<%>-Instance -Symbol -String -String
   []
   #:background -Boolean #f
   -Void)]
 [color-prefs:marshall-style-delta
  (-Style-Delta%-Instance . -> . -Printable)]
 [color-prefs:unmarshall-style-delta
  (-Printable . -> . (-opt -Style-Delta%-Instance))]
 [color-prefs:white-on-black (-> -Void)]
 [color-prefs:black-on-white (-> -Void)]
 [color-prefs:add-color-scheme-entry
  (->optkey
   -Symbol (Un -String -Color%-Instance) (Un -String -Color%-Instance)
   []
   #:style (-opt -String) #f
   #:bold? -Boolean #f
   #:underline? -Boolean #f
   #:italic? -Boolean #f
   #:background (Un -False -String -Color%-Instance) #f
   -Void)]
 [color-prefs:add-color-scheme-preferences-panel
  (->optkey
   []
   #:extras ((make-Instance -Panel%) . -> . Univ) #f
   -Void)]
 [color-prefs:register-info-based-color-schemes (-> -Void)]
 [color-prefs:set-current-color-scheme (-Symbol . -> . -Void)]
 [color-prefs:get-current-color-scheme-name (-> -Color-Prefs:Color-Scheme-Style-Name)]
 [#:opaque Color-Prefs:Known-Color-Scheme-Name color-prefs:known-color-scheme-name?]
 [#:opaque Color-Prefs:Color-Scheme-Style-Name color-prefs:color-scheme-style-name?]
 [color-prefs:lookup-in-color-scheme
  ;; TODO less precise than doc
  (-Color-Prefs:Known-Color-Scheme-Name . -> . (Un -Style-Delta%-Instance -Color%-Instance))]
 [color-prefs:set-in-color-scheme
  (-Color-Prefs:Known-Color-Scheme-Name
   (Un -Style-Delta%-Instance
       -Color%-Instance)
   . -> . -Void)]
 [color-prefs:register-color-scheme-entry-change-callback
  (->opt
   -Color-Prefs:Known-Color-Scheme-Name
   ((Un -Style-Delta%-Instance -Color%-Instance) . -> . Univ)
   [-Boolean]
   -Void)]
 [color-prefs:get-color-scheme-names
  (-> (-values (list (-set Univ) (-set Univ))))] ; TODO doc doesn't say set of what
 ;; 7 Color
 [color:text-mixin (parse-type #'Color:Text-Mixin)]
 [color:text% (parse-type #'Color:Text%)]
 [color:text-mode-mixin (parse-type #'Color:Text-Mode-Mixin)]
 [color:text-mode% (parse-type #'Color:Text-Mode%)]
 [color:get-parenthesis-colors-table
  (-> (-lst (-Tuple (list -Symbol
                             -String
                             (-vec -Color%-Instance)
                             (Un (-val 'low) (-val 'high))))))]
 [color:misspelled-text-color-style-name (-> -String)]
 ;; 8 Comment Box
 [comment-box:snip% (parse-type #'Comment-Box:Snip%)]
 [comment-box:snipclass (make-Instance -Snip-Class%)]
 ;; 9 Decorated Editor Snip
 ; TODO: all syntax, and docs suggests they're deprecated?
 ;; 10 Editor Snip
 [editor-snip:decorated-mixin (parse-type #'Editor-Snip:Decorated-Mixin)]
 [editor-snip:decorated% (parse-type #'Editor-Snip:Decorated%)]
 [editor-snip:decorated-snipclass% (parse-type #'Editor-Snip:Decorated-Snipclass%)]
  ;; 11 Editor
 [editor:basic-mixin (parse-type #'Editor:Basic-Mixin)]
 [editor:standard-style-list-mixin (parse-type #'Editor:Standard-Style-List-Mixin)]
 [editor:keymap-mixin (parse-type #'Editor:Keymap-Mixin)]
 [editor:autowrap-mixin (parse-type #'Editor:Autowrap-Mixin)]
 [editor:file-mixin (parse-type #'Editor:File-Mixin)]
 [editor:backup-autosave-mixin (parse-type #'Editor:Backup-Autosave-Mixin)]
 [editor:info-mixin (parse-type #'Editor:Info-Mixin)]
 [editor:set-current-preferred-font-size (-Nat . -> . -Void)]
 [editor:get-current-preferred-font-size (-> -Nat)]
 [editor:font-size-pref->current-font-size
  ((make-HeterogeneousVector (list (-HT (-ne-lst (-Tuple (list -Nat -Nat))) -Nat)
                                   -Nat))
   . -> .
   -Nat)]
 [editor:get-change-font-size-when-monitors-change? (-> -Boolean)]
 [editor:set-change-font-size-when-monitors-change? (-Boolean . -> . -Void)]
 [editor:set-default-font-color
  (->opt
   -Color%-Instance
   [(-opt -Color%-Instance)]
   -Void)]
 [editor:get-default-color-style-name (-> -String)]
 [editor:set-standard-style-list-delta
  (-String -Style-Delta%-Instance . -> . -Void)]
 [editor:set-standard-style-list-pref-callbacks (-> Univ)]
 [editor:get-standard-style-list
  (-> (make-Instance (parse-type #'Style-List%)))]
 [editor:add-after-user-keymap
  (-Keymap%-Instance (-lst -Keymap%-Instance)
   . -> . (-lst -Keymap%-Instance))]
 ;; 12 Exit
 [exit:exiting? (-> -Boolean)]
 [exit:set-exiting (-Boolean . -> . -Void)]
 [exit:insert-on-callback ((-> -Void) . -> . (-> -Void))]
 [exit:insert-can?-callback ((-> -Boolean) . -> . (-> -Void))]
 [exit:can-exit? (-> -Boolean)]
 [exit:on-exit (-> -Void)]
 [exit:exit (-> -Void)]
 [exit:user-oks-exit (-> -Boolean)]
 ;; 13 Finder
 [finder:dialog-parent-parameter
  (-Param (Un -False -Dialog%-Instance -Frame%-Instance))]
 [finder:default-extension (-Param -String)]
 [finder:default-filters
  (-Param (-lst (-Tuple (list -String -String))))]

 [finder:common-put-file (->opt
                          [-String
                           (-opt -Path)
                           -Boolean
                           -String
                           (-opt -Byte-Regexp)
                           -String
                           (-opt -Top-Level-Window<%>-Instance)]
                          (-opt -Path))]
 [finder:common-get-file (->opt
                          [(-opt -Path)
                           -String
                           (-opt -Byte-Regexp)
                           -String
                           (-opt -Top-Level-Window<%>-Instance)]
                          (-opt -Path))]
 [finder:std-put-file (->opt
                       [-String
                        (-opt -Path)
                        -Boolean
                        -String
                        (-opt -Byte-Regexp)
                        -String
                        (-opt -Top-Level-Window<%>-Instance)]
                       (-opt -Path))]
 [finder:std-get-file (->opt
                       [(-opt -Path)
                        -String
                        (-opt -Byte-Regexp)
                        -String
                        (-opt -Top-Level-Window<%>-Instance)]
                       (-opt -Path))]
 [finder:put-file (->opt
                   [-String
                    (-opt -Path)
                    -Boolean
                    -String
                    (-opt -Byte-Regexp)
                    -String
                    (-opt -Top-Level-Window<%>-Instance)]
                   (-opt -Path))]
 [finder:get-file (->opt
                   [(-opt -Path)
                    -String
                    (Un -Byte-Regexp -String -False)
                    -String
                    (-opt -Top-Level-Window<%>-Instance)]
                   (-opt -Path))]
 ;; 14 Frame
 [frame:basic-mixin (parse-type #'Frame:Basic-Mixin)]
 [frame:focus-table-mixin (parse-type #'Frame:Focus-Table-Mixin)]
 [frame:size-pref-mixin (parse-type #'Frame:Size-Pref-Mixin)]
 [frame:register-group-mixin (parse-type #'Frame:Register-Group-Mixin)]
 [frame:status-line-mixin (parse-type #'Frame:Status-Line-Mixin)]
 [frame:info-mixin (parse-type #'Frame:Info-Mixin)]
 [frame:text-info-mixin (parse-type #'Frame:Text-Info-Mixin)]
 [frame:pasteboard-info-mixin (parse-type #'Frame:Pasteboard-Info-Mixin)]
 [frame:standard-menus-mixin (parse-type #'Frame:Standard-Menus-Mixin)]
 [frame:editor-mixin (parse-type #'Frame:Editor-Mixin)]
 [frame:text-mixin (parse-type #'Frame:Text-Mixin)]
 [frame:pasteboard-mixin (parse-type #'Frame:Pasteboard-Mixin)]
 [frame:delegate-mixin (parse-type #'Frame:Delegate-Mixin)]
 [frame:searchable-mixin (parse-type #'Frame:Searchable-Mixin)]
 [frame:searchable-text-mixin (parse-type #'Frame:Searchable-Text-Mixin)]
 [frame:basic% (parse-type #'Frame:Basic%)]
 [frame:size-pref% (parse-type #'Frame:Size-Pref%)]
 [frame:info% (parse-type #'Frame:Info%)]
 [frame:text-info% (parse-type #'Frame:Info%)]
 [frame:pasteboard-info% (parse-type #'Frame:Pasteboard-Info%)]
 [frame:status-line% (parse-type #'Frame:Status-Line%)]
 [frame:standard-menus% (parse-type #'Frame:Standard-Menus%)]
 [frame:editor% (parse-type #'Frame:Editor%)]
 [frame:text% (parse-type #'Frame:Text%)]
 [frame:searchable% (parse-type #'Frame:Searchable%)]
 [frame:delegate% (parse-type #'Frame:Delegate%)]
 [frame:pasteboard% (parse-type #'Frame:Pasteboard%)]
 [frame:setup-size-pref
  (->optkey
   -Symbol -NonNegReal -NonNegReal
   []
   #:maximized? -Boolean #f
   #:position-preferences (-opt -Symbol) #f
   -Void)]
 [frame:add-snip-menu-items (->opt -Menu%-Instance
                                   -Menu-Item% ; FIXME (subclass?/c menu-item%)
                                   [((make-Instance -Menu-Item%) . -> . -Void)]
                                   -Void)]
 [frame:reorder-menus (-Frame%-Instance . -> . -Void)]
 [frame:remove-empty-menus (-Frame%-Instance . -> . -Void)]
 [frame:current-icon
  (-Param  (Un -False -Bitmap%-Instance (-pair -Bitmap%-Instance -Bitmap%-Instance)))]
 [frame:lookup-focus-table
  (->opt [-Eventspace] (-lst (make-Instance (parse-type #'Frame:Focus-Table<%>))))]
 ;; 15 Group
 [group:% (parse-type #'Group:%)]
 [group:get-the-frame-group (-> (make-Instance (parse-type #'Group:%)))]
 [group:on-close-action (-> -Void)]
 [group:can-close-check (-> -Boolean)]
 [group:add-to-windows-menu ((-Menu%-Instance . -> . Univ) . -> . Univ)]
 [group:create-windows-menu
  ((make-Instance -Menu-Item-Container<%>) . -> . -Menu%-Instance)]
 ;; 16 GUI Utilities
 [gui-utils:trim-string (-String -PosInt . -> . -String)]
 [gui-utils:quote-literal-label
  (->optkey -String [] #:quote-amp? Univ #f -String)]
 [gui-utils:format-literal-label ((list -String) Univ . ->* . -String)]
 [gui-utils:cancel-on-right? (-> -Boolean)]
 [gui-utils:ok/cancel-buttons
  (->optkey
   -Area-Container<%>-Instance
   (-Button%-Instance -Event%-Instance . -> . Univ)
   (-Button%-Instance -Event%-Instance . -> . Univ)
   [-String -String]
   #:confirm-style (-lst -Symbol) #f
   (-values (list -Button%-Instance -Button%-Instance)))]
 [gui-utils:next-untitled-name (-> -String)]
 [gui-utils:cursor-delay (cl-> [{} -Real]
                               [{-Real} -Void])]
 [gui-utils:show-busy-cursor (->opt (-> Univ) [-Integer] Univ)]
 [gui-utils:delay-action (-Real (-> -Void) (-> -Void) . -> . (-> -Void))]
 [gui-utils:local-busy-cursor
  (->opt -Window<%>-Instance (-> Univ) [-Integer] Univ)]
 [gui-utils:unsaved-warning
  (->opt -String -String
         [-Boolean
          (Un -False -Frame%-Instance -Dialog%-Instance)
          -Boolean]
         (Un (-val 'continue) (-val 'save) (-val 'cancel)))]

 [gui-utils:get-choice
  (->opt -String -String -String
         [-String
          Univ
          (Un -False -Frame%-Instance -Dialog%-Instance)
          (Un (-val 'app) (-val 'caution) (-val 'stop))
          (-opt (cl-> [{-Boolean} -Void]
                      [{} -Boolean]))
          -String]
         Univ)]
 [gui-utils:get-clicked-clickback-delta
  (->opt [-Style-Delta%-Instance] -Boolean)]
 [gui-utils:get-clickback-delta
  (->opt [-Style-Delta%-Instance] -Boolean)]
 ;; 17 Handler
 [#:opaque Handler:Handler handler:handler?]
 [handler:handler-name (-Handler:Handler . -> . -String)]
 [handler:handler-extension
  (-Handler:Handler . -> . (Un (-Path . -> . -Boolean) (-lst -String)))]
 [handler:handler-handler
  (-Handler:Handler . -> . (-Path . -> . -Frame:Editor<%>-Instance))]
 [handler:insert-format-handler
  (-String
   (Un -String (-lst -String) (-Path . -> . -Boolean))
   (-Path . -> . (-opt -Frame:Editor<%>-Instance))
   . -> . -Void)]
 [handler:find-named-format-handler
  (-String . -> . (-Path . -> . -Frame:Editor<%>-Instance))]
 [handler:find-format-handler
  (-Path . -> . (-Path . -> . -Frame:Editor<%>-Instance))]
 [handler:edit-file
  ((-opt -Frame:Editor<%>-Instance)
   [(-> -Frame:Editor<%>-Instance)]
   . ->opt . (-opt -Frame:Editor<%>-Instance))]
 [handler:current-create-new-window
  (-Param ((-opt -Path) . -> . -Frame%-Instance))]
 [handler:open-file ([(Un -False -Path -String)]
                     . ->opt . (-opt (make-Instance (parse-type #'Frame:Basic<%>))))]
 [handler:install-recent-items (-Menu%-Instance . -> . -Void)]
 [handler:set-recent-items-frame-superclass
  ;; FIXME (implementation?/c frame:standard-menus<%>)
  ((parse-type #'Frame:Standard-Menus<%>) . -> . -Void)]
 [handler:add-to-recent (-Path . -> . -Void)]
 [handler:set-recent-position (-Path -Nat -Nat . -> . -Void)]
 [handler:size-recently-opened-files (-Nat . -> . -Void)]
 ;; 18 Icon
 [icon:get-paren-highlight-bitmap (-> -Bitmap%-Instance)]
 [icon:get-eof-bitmap (-> -Bitmap%-Instance)]
 [icon:get-autowrap-bitmap (-> -Bitmap%-Instance)]
 [icon:get-lock-bitmap (-> -Bitmap%-Instance)]
 [icon:get-unlock-bitmap (-> -Bitmap%-Instance)]
 [icon:get-anchor-bitmap (-> -Bitmap%-Instance)]
 [icon:get-left/right-cursor (-> -Cursor%-Instance)]
 [icon:get-up/down-cursor (-> -Cursor%-Instance)]
 [icon:get-gc-on-bitmap (-> -Bitmap%-Instance)]
 [icon:get-gc-off-bitmap (-> -Bitmap%-Instance)]
 ;; 19 Keymap
 [keymap:aug-keymap-mixin (parse-type #'Keymap:Aug-Keymap-Mixin)]
 [keymap:aug-keymap% (parse-type #'Keymap:Aug-Keymap%)]
 [keymap:remove-user-keybindings-file (Univ . -> . Univ)]
 [keymap:add-user-keybindings-file (Univ . -> . Univ)]
 [keymap:add-to-right-button-menu
  (-Param  ((make-Instance -Popup-Menu%)
            -Editor<%>-Instance
            -Event%-Instance
            . -> . -Void))]
 [keymap:add-to-right-button-menu/before
  (-Param  ((make-Instance -Popup-Menu%)
            -Editor<%>-Instance
            -Event%-Instance
            . -> . -Void))]
 [keymap:call/text-keymap-initializer ((-> Univ) . -> . Univ)]
 [keymap:canonicalize-keybinding-string (-String . -> . -String)]
 [keymap:get-editor (-> -Keymap%-Instance)]
 [keymap:get-file (-> -Keymap%-Instance)]
 [keymap:get-user (-> -Keymap%-Instance)]
 [keymap:get-global (-> -Keymap%-Instance)]
 [keymap:get-search (-> -Keymap%-Instance)]
 [keymap:make-meta-prefix-list (-String [-Boolean] . ->opt . (-lst -String))]
 [keymap:send-map-function-meta
  (-Keymap%-Instance
   -String
   -String
   [-Boolean]
   #:alt-as-meta-keymap (-opt -Keymap%-Instance) #f
   . ->optkey . -Void)]
 [keymap:setup-editor (-Keymap%-Instance . -> . -Void)]
 [keymap:setup-file (-Keymap%-Instance . -> . -Void)]
 [keymap:setup-global (-Keymap%-Instance . -> . -Void)]
 [keymap:setup-search (-Keymap%-Instance . -> . -Void)]
 [keymap:set-chained-keymaps ((make-Instance (parse-type #'Keymap:Aug-Keymap<%>))
                              (-lst -Keymap%-Instance)
                              . -> . -Void)]
 [keymap:remove-chained-keymap (-Editor<%>-Instance
                                (make-Instance (parse-type #'Keymap:Aug-Keymap<%>))
                                . -> . -Void)]
 [keymap:region-click (Univ
                       Univ
                       (-NonNegReal -Boolean -NonNegReal -NonNegReal . -> . Univ)
                       . -> . Univ)]
 ;; 20 Menu
 [menu:can-restore-mixin (parse-type #'Menu:Can-Restore-Mixin)]
 [menu:can-restore-underscore-mixin (parse-type #'Menu:Can-Restore-Underscore-Mixin)]
 [menu:can-restore-menu-item% (parse-type #'Menu:Can-Restore-Menu-Item%)]
 [menu:can-restore-checkable-menu-item% (parse-type #'Menu:Can-Restore-Checkable-Menu-Item%)]
 [menu:can-restore-underscore-menu% (parse-type #'Menu:Can-Restore-Underscore-Menu%)]
 ;; 21 Mode
 [mode:surrogate-text% (parse-type #'Mode:Surrogate-Text%)]
 [mode:host-text-mixin (parse-type #'Mode:Host-Text-Mixin)]
 ;; 22 Number Snip
 [number-snip:snip-class% (parse-type #'Number-Snip:Snip-Class%)]
 [number-snip:make-repeating-decimal-snip (-Real -Boolean . -> . -Snip%-Instance)]
 [number-snip:make-fraction-snip (-Real -Boolean . -> . -Snip%-Instance)]
 ;; 23 Panel
 [panel:single-mixin (parse-type #'Panel:Single-Mixin)]
 [panel:single-window-mixin (parse-type #'Panel:Single-Window-Mixin)]
 [panel:single% (parse-type #'Panel:Single%)]
 [panel:single-pane% (parse-type #'Panel:Single-Pane%)]
 [panel:dragable-mixin (parse-type #'Panel:Dragable-Mixin)]
 [panel:vertical-dragable-mixin (parse-type #'Panel:Vertical-Dragable-Mixin)]
 [panel:horizontal-dragable-mixin (parse-type #'Panel:Horizontal-Dragable-Mixin)]
 [panel:vertical-dragable% (parse-type #'Panel:Vertical-Dragable%)]
 [panel:horizontal-dragable% (parse-type #'Panel:Horizontal-Dragable%)]
 [panel:splitter-mixin (parse-type #'Panel:Splitter-Mixin)]
 [panel:discrete-sizes-mixin (parse-type #'Panel:Discrete-Sizes-Mixin)]
 [panel:horizontal-discrete-sizes% (parse-type #'Panel:Horizontal-Discrete-Sizes%)]
 [panel:vertical-discrete-sizes% (parse-type #'Panel:Vertical-Discrete-Sizes%)]
 [panel:dragable-container-size
  ((-lst (-Tuple (list -Real -Real -Boolean -Boolean)))
   -Real
   -Boolean
   . -> . (-values (list -Real -Real)))]
 [panel:dragable-place-children
  ((-lst (-Tuple (list -Real -Real -Boolean -Boolean)))
   -Real
   -Real
   (-lst -Real #|between/c 0 1|#)
   -Real
   -Boolean
   . -> . (-values (list (-lst (-Tuple (list -Nat -Nat -Nat -Nat)))
                         (-lst (-Tuple (list -Nat -Nat))))))]
 ;; 24 Pasteboard
 [pasteboard:basic% (parse-type #'Pasteboard:Basic%)]
 [pasteboard:standard-style-list% (parse-type #'Pasteboard:Standard-Style-List%)]
 [pasteboard:keymap% (parse-type #'Pasteboard:Keymap%)]
 [pasteboard:file% (parse-type #'Pasteboard:File%)]
 [pasteboard:backup-autosave% (parse-type #'Pasteboard:Backup-Autosave%)]
 [pasteboard:info% (parse-type #'Pasteboard:Info%)]
 ;; 25 Path Utils
 [path-utils:generate-autosave-name ((Un -False -Pathlike -SomeSystemPath) . -> . -String)]
 [path-utils:generate-backup-name (-Path . -> . -Path)]
 ;; 26 Preferences
 [preferences:put-preferences/gui ((-lst -Symbol) (-lst Univ) . -> . Univ)]
 [preferences:get-preference/gui (-Symbol [(-> -Void)] . ->opt . Univ)]
 [preferences:add-panel
  ((Un -String (-ne-lst -String))
   (-Area-Container-Window<%>-Instance . -> . -Area-Container-Window<%>-Instance)
   . -> . Univ)]
 [preferences:add-editor-checkbox-panel (-> -Void)]
 [preferences:add-general-checkbox-panel (-> -Void)]
 [preferences:add-warnings-checkbox-panel (-> -Void)]
 [preferences:add-scheme-checkbox-panel (-> -Void)]
 [preferences:add-to-warnings-checkbox-panel
  ((-Vertical-Panel%-Instance . -> . -Void) . -> . -Void)]
 [preferences:add-to-scheme-checkbox-panel
  ((-Vertical-Panel%-Instance . -> . -Void) . -> . -Void)]
 [preferences:add-to-editor-checkbox-panel
  ((-Vertical-Panel%-Instance . -> . -Void) . -> . -Void)]
 [preferences:add-to-general-checkbox-panel
  ((-Vertical-Panel%-Instance . -> . -Void) . -> . -Void)]
 [preferences:add-font-panel (-> -Void)]
 [preferences:show-dialog (-> -Void)]
 [preferences:hide-dialog (-> -Void)]
 [preferences:add-on-close-dialog-callback ((-> -Void) . -> . -Void)]
 [preferences:add-can-close-dialog-callback ((-> -Boolean) . -> . -Void)]
 [preferences:add-check
  (-Area-Container<%>-Instance
   -Symbol
   -String
   [(-Boolean . -> . Univ)
    (Univ . -> . -Boolean)]
   . ->opt . -Void)]
 ;; 27 Preferences, Textual
 [preferences:get (-> -Symbol -Sexp)]
 [preferences:set (-> -Symbol -Sexp -Void)]
 [preferences:add-callback (-Symbol (-Symbol Univ . -> . Univ) [-Boolean] . ->opt . (-> -Void))]
 [preferences:set-default
  (->optkey
   -Symbol -Sexp (-> Univ -Boolean)
   []
   #:aliases (-lst -Symbol) #f
   #:rewrite-aliases (-lst (-> Univ Univ)) #f
   -Void)]
 [preferences:default-set? (-Symbol . -> . -Boolean)]
 [preferences:set-un/marshall
  (-Symbol (Univ . -> . -Printable) (-Printable . -> . Univ) . -> . -Void)]
 [preferences:restore-defaults (-> -Void)]
 [preferences:register-save-callback ((-Boolean . -> . Univ) . -> . -Symbol)]
 [preferences:unregister-save-callback (-Symbol . -> . -Void)]
 [exn:make-unknown-preference (-String -Continuation-Mark-Set . -> . -Exn:Unknown-Preference)]
 [#:opaque Exn:Unknown-Preference exn:unknown-preference?]
 [exn:struct:unknown-preference -Struct]
 [preferences:low-level-put-preferences
  (-Param ((-lst -Symbol) (-lst Univ) . -> . Univ))]
 [preferences:low-level-get-preference
  (-Param (-Symbol [(-> Univ)] . ->opt . Univ))]
 [#:opaque Preferences:Snapshot preferences:snapshot?]
 [preferences:restore-prefs-snapshot (-Preferences:Snapshot . -> . -Void)]
 [preferences:get-prefs-snapshot (-> -Preferences:Snapshot)]
 ;; 28 Racket
 [racket:sexp-snip% (parse-type #'Racket:Sexp-Snip%)]
 [racket:text-mixin (parse-type #'Racket:Text-Mixin)]
 [racket:text-mode-mixin (parse-type #'Racket:Text-Mode-Mixin)]
 [racket:set-mode-mixin (parse-type #'Racket:Set-Mode-Mixin)]
 [racket:text% (parse-type #'Racket:Text%)]
 [racket:text-mode% (parse-type #'Racket:Text-Mode%)]
 [racket:text-balanced? ((make-Instance -Text%) [-Nat (-opt -Nat)] . ->opt . -Boolean)]
 [racket:add-preferences-panel (-> -Void)]
 [racket:get-keymap (-> -Keymap%-Instance)]
 [racket:add-coloring-preferences-panel (-> Univ)]
 [racket:get-color-prefs-table
  (-> (-lst (-Tuple (list -Symbol -Color%-Instance -String))))]
 [racket:get-white-on-black-color-prefs-table
  (-> (-lst (-Tuple (list -Symbol -Color%-Instance -String))))]
 [racket:short-sym->pref-name (-Symbol . -> . -Symbol)]
 [racket:short-sym->style-name (-String . -> . -Symbol)]
 [racket:get-wordbreak-map (-> (make-Instance -Editor-Wordbreak-Map%))]
 [racket:init-wordbreak-map (-Keymap%-Instance . -> . -Void)]
 [racket:setup-keymap (-Keymap%-Instance . -> . -Void)]
 ;; 29 Text
 [text:basic-mixin (parse-type #'Text:Basic-Mixin)]
 [text:line-spacing-mixin (parse-type #'Text:Line-Spacing-Mixin)]
 [text:first-line-mixin (parse-type #'Text:First-Line-Mixin)]
 [text:foreground-color-mixin (parse-type #'Text:Foreground-Color-Mixin)]
 [text:hide-caret/selection-mixin (parse-type #'Text:Hide-Caret/Selection-Mixin)]
 [text:nbsp->space-mixin (parse-type #'Text:Nbsp->Space-Mixin)]
 [text:column-guide-mixin (parse-type #'Text:Column-Guide-Mixin)]
 [text:normalize-paste-mixin (parse-type #'Text:Normalize-Paste-Mixin)]
 [text:searching-mixin (parse-type #'Text:Searching-Mixin)]
 [text:return-mixin (parse-type #'Text:Return-Mixin)]
 [text:wide-snip-mixin (parse-type #'Text:Wide-Snip-Mixin)]
 [text:1-pixel-string-snip% (parse-type #'Text:1-Pixel-String-Snip%)]
 [text:1-pixel-tab-snip% (parse-type #'Text:1-Pixel-Tab-Snip%)]
 [text:delegate-mixin (parse-type #'Text:Delegate-Mixin)]
 [text:info-mixin (parse-type #'Text:Info-Mixin)]
 [text:clever-file-format-mixin (parse-type #'Text:Clever-File-Format-Mixin)]
 [text:crlf-line-endings-mixin (parse-type #'Text:Crlf-Line-Endings-Mixin)]
 [text:file-mixin (parse-type #'Text:File-Mixin)]
 [text:ports-mixin (parse-type #'Text:Ports-Mixin)]
 [text:input-box-mixin (parse-type #'Text:Input-Box-Mixin)]
 [text:autocomplete-mixin (parse-type #'Text:Autocomplete-Mixin)]
 [text:basic% (parse-type #'Text:Basic%)]
 [text:line-spacing% (parse-type #'Text:Line-Spacing%)]
 [text:hide-caret/selection% (parse-type #'Text:Hide-Caret/Selection%)]
 [text:nbsp->space% (parse-type #'Text:Nbsp->Space%)]
 [text:normalize-paste% (parse-type #'Text:Normalize-Paste%)]
 [text:delegate% (parse-type #'Text:Delegate%)]
 [text:wide-snip% (parse-type #'Text:Wide-Snip%)]
 [text:standard-style-list% (parse-type #'Text:Standard-Style-List%)]
 [text:input-box% (parse-type #'Text:Input-Box%)]
 [text:keymap% (parse-type #'Text:Keymap%)]
 [text:return% (parse-type #'Text:Return%)]
 [text:autowrap% (parse-type #'Text:Autowrap%)]
 [text:file% (parse-type #'Text:File%)]
 [text:clever-file-format% (parse-type #'Text:Clever-File-Format%)]
 [text:backup-autosave% (parse-type #'Text:Backup-Autosave%)]
 [text:searching% (parse-type #'Text:Searching%)]
 [text:info% (parse-type #'Text:Info%)]
 [text:line-numbers-mixin (parse-type #'Text:Line-Numbers-Mixin)]
 [#:opaque Text:Range text:range?]
 [text:range-start (-Text:Range . -> . -Nat)]
 [text:range-end (-Text:Range . -> . -Nat)]
 [text:range-caret-space? (-Text:Range . -> . -Boolean)]
 [text:range-style (-Text:Range . -> . -Nat)] ; FIXME: doc says nat...
 [text:range-color (-Text:Range . -> . (Un -String -Color%-Instance))]
 [text:autocomplete-append-after (-Param -String)]
 [text:autocomplete-limit (-Param -PosInt)]
 [text:get-completions/manuals ((-opt (-lst -Symbol)) . -> . (-lst -String))]
 [text:lookup-port-name ((-opt (make-Instance (parse-type #'Editor:Basic<%>))) . -> . -Symbol)]
 ;; 30 Splash
 [start-splash
  ((Un -Pathlike
       -Bitmap%-Instance
       (make-HeterogeneousVector
        (list
         (Un (-DC<%>-Instance . -> . -Void)
             (-DC<%>-Instance -Nat -Nat -Nat -Nat . -> . -Void))
         -Nat
         -Nat)))
   -String
   -Nat
   []
   #:allow-funny? -Boolean #f
   #:frame-icon (Un -False
                    -Bitmap%-Instance
                    (-pair -Bitmap%-Instance -Bitmap%-Instance)) #f
   . ->optkey . -Void)]
 [shutdown-splash (-> -Void)]
 [close-splash (-> -Void)]
 [add-splash-icon (-Bitmap%-Instance -Nat -Nat . -> . -Void)]
 [get-splash-bitmap (-> (-opt -Bitmap%-Instance))]
 [set-splash-bitmap (-Bitmap%-Instance . -> . -Void)]
 [get-splash-canvas (-> -Canvas%-Instance)]
 [get-splash-eventspace (-> -Eventspace)]
 [get-splash-paint-callback
  (-> (-DC<%>-Instance -Nat -Nat -Nat -Nat . -> . -Void))]
 [set-splash-paint-callback
  ((-> (-DC<%>-Instance -Nat -Nat -Nat -Nat . -> . -Void)) . -> . -Void)]
 [set-splash-progress-bar?! (-Boolean . -> . -Void)]
 [set-splash-char-observer ((-Key-Event%-Instance . -> . Univ) . -> . -Void)]
 [set-splash-event-callback ((-Mouse-Event%-Instance . -> . Univ) . -> . -Void)]
 [get-splash-event-callback (-> (-Mouse-Event%-Instance . -> . Univ))]
 [set-refresh-splash-on-gauge-change?! ((-Nat -Nat . -> . Univ) . -> . -Void)]
 [get-splash-width (-> -Nat)]
 [get-splash-height (-> -Nat)]
 [refresh-splash (-> -Void)]
 ;; 31 Test
 [test:button-push ((Un -String -Button%-Instance) . -> . -Void)]
 [test:set-radio-box!
  ((Un -String -Regexp (make-Instance -Radio-Box%)) (Un -String -Nat) . -> . -Void)]
 [test:set-radio-box-item! ((Un -String -Regexp) . -> . -Void)]
 [test:set-check-box! ((Un -String (make-Instance -Check-Box%)) -Boolean . -> . -Void)]
 [test:set-choice! ((Un -String (make-Instance -Choice%))
                    (Un -String -PosInt)
                    . -> . -Void)]
 [test:set-list-box! ((Un -String (make-Instance -List-Box%))
                      (Un -String -Nat)
                      . -> . -Void)]
 [test:keystroke ((Un -Char -Symbol)
                  [(-lst (Un (-val 'alt) (-val 'control) (-val 'meta) (-val 'shift)
                                (-val 'noalt) (-val 'nocontrol) (-val 'nometa) (-val 'noshift)))]
                  . ->opt . -Void)]
 [test:menu-select ((list -String) (-lst -String) . ->* . -Void)]
 [test:mouse-click ((Un (-val 'left) (-val 'middle) (-val 'right))
                    -Int
                    -Int
                    [(-lst (Un (-val 'alt) (-val 'control) (-val 'meta) (-val 'shift)
                                  (-val 'noalt) (-val 'nocontrol) (-val 'nometa) (-val 'noshift)))]
                    . ->opt . -Void)]
 [test:run-interval (cl-> [{-Real} -Void]
                          [{} -Real])]
 [test:current-get-eventspaces (-Param (-> (-lst -Eventspace)))]
 [test:new-window (-Window<%>-Instance . -> . -Void)]
 [test:close-top-level-window (-Top-Level-Window<%>-Instance . -> . -Void)]
 [test:top-level-focus-window-has? ((-Area<%>-Instance . -> . -Boolean) . -> . -Boolean)]
 [test:number-pending-actions (-> -Nat)]
 [test:reraise-error (-> -Void)]
 [test:run-one ((-> -Void) . -> . -Void)]
 [test:use-focus-table (-Param (Un -Boolean (-val 'debug)))]
 [test:get-active-top-level-window (-> (Un -False -Frame%-Instance -Dialog%-Instance))]
 [label-of-enabled/shown-button-in-top-level-window? (-String . -> . -Boolean)]
 [enabled-shown-button? (-Button%-Instance . -> . -Boolean)]
 [button-in-top-level-focusd-window? (-Button%-Instance . -> . -Boolean)]
 ;; 32 Version
 [version:add-spec (Univ Univ . -> . -Void)]
 [version:version (-> -String)]
 ;; 33 Backwards Compatibility
 [scheme:text-mixin (parse-type #'Racket:Text-Mixin)]
 [scheme:text% (parse-type #'Racket:Text%)]
 [scheme:text-mode-mixin (parse-type #'Racket:Text-Mode-Mixin)]
 [scheme:text-mode% (parse-type #'Racket:Text-Mode%)]
 [scheme:set-mode-mixin (parse-type #'Racket:Set-Mode-Mixin)]
 [scheme:sexp-snip% (parse-type #'Racket:Sexp-Snip%)]
 ; other ones not obvious. Maybe just drop compatibility for now? ^^
 ;; 34 Signatures
 ;; 35 Unit
 )
