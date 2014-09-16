#lang typed/racket

;; Types for the framework library

(require "../racket/private/gui-types.rkt")

;; Frequently reused types
(define-type Style-Delta%-Instance (Instance Style-Delta%))
(define-type Snip%-Instance (Instance Snip%))
(define-type Color%-Instance (Instance Color%))
(define-type Frame:Basic<%>-Instance (Instance Frame:Basic<%>))
(define-type Canvas%-Instance (Instance Canvas%))
(define-type Menu%-Instance (Instance Menu%))
(define-type Key-Event%-Instance (Instance Key-Event%))
(define-type Mouse-Event%-Instance (Instance Mouse-Event%))
(define-type Control-Event%-Instance (Instance Control-Event%))
(define-type Menu-Item%-Instance (Instance Menu-Item%))
(define-type Canvas:Basic<%>-Instance (Instance Canvas:Basic<%>))
(define-type Text%-Instance (Instance Text%))
(define-type Editor-Canvas%-Instance (Instance Editor-Canvas%))
(define-type Editor<%>-Instance (Instance Editor<%>))
(define-type Area-Container<%>-Instance (Instance Area-Container<%>))
(define-type Dialog%-Instance (Instance Dialog%))

;; 3 Autosave
(provide Autosave:Autosavable<%>
         Editor<%>-And-Autosave:Autosavable<%>)

(define-type Autosave:Autosavable<%>
  (Class [do-autosave (-> (Option Path))]))

(define-type Editor<%>-And-Autosave:Autosavable<%>
  (Class #:implements Autosave:Autosavable<%>
         #:implements Editor<%>))

;; 4 Canvas
(provide Canvas:Basic<%>
         Canvas:Basic-Mixin
         Canvas:Basic%
         Canvas:Color<%>
         Canvas:Color-Mixin
         Canvas:Color%
         Canvas:Delegate<%>
         Canvas:Delegate-Mixin
         Canvas:Delegate%
         Canvas:Info<%>
         Canvas:Info-Mixin
         Canvas:Info%
         Canvas:Wide-Snip<%>
         Canvas:Wide-Snip-Mixin
         Canvas:Wide-Snip%)

;; TODO: I'm not sure about interface for classes

(define-type Canvas:Basic<%>
  (Class #:implements Editor-Canvas%))

(define-type Canvas:Basic-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Editor-Canvas%)
    ->
    (Class #:row-var r #:implements Canvas:Basic<%>)))

(define-type Canvas:Basic%
  (Class #:implements Canvas:Basic<%>
         (init [parent Dialog%-Instance]
               [editor (Instance Text:Basic<%>)])))

(define-type Canvas:Color<%>
  (Class #:implements Canvas:Basic<%>))

(define-type Canvas:Color-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Canvas:Basic<%>)
    ->
    (Class #:row-var r #:implements Canvas:Color<%>)))

(define-type Canvas:Color%
  (Class #:implements Canvas:Color<%>))

(define-type Canvas:Delegate<%>
  (Class #:implements Canvas:Basic<%>))

(define-type Canvas:Delegate-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Canvas:Basic<%>)
    ->
    (Class #:row-var r #:implements Canvas:Delegate<%>)))

(define-type Canvas:Delegate%
  (Class #:implements Canvas:Delegate<%>))

(define-type Canvas:Info<%>
  (Class #:implements Canvas:Basic<%>))

(define-type Canvas:Info-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Canvas:Basic<%>)
    ->
    (Class #:row-var r #:implements Canvas:Info<%>)))

(define-type Canvas:Info%
  (Class #:implements Canvas:Info<%>))

(define-type Canvas:Wide-Snip<%>
  (Class #:implements Canvas:Basic<%>
         [recalc-snips (-> Void)]
         [add-wide-snip (Snip%-Instance -> Void)]
         [add-tall-snip (Snip%-Instance -> Void)]))

(define-type Canvas:Wide-Snip-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Canvas:Basic<%>)
    ->
    (Class #:row-var r #:implements Canvas:Wide-Snip<%>)))

(define-type Canvas:Wide-Snip%
  (Class #:implements Canvas:Wide-Snip<%>))

;; 7 Color
(provide Color:Text<%>
         Color:Text%
         Color:Text-Mixin
         Color:Text-Mode<%>
         Color:Text-Mode%
         Color:Text-Mode-Mixin)

(define-type Color:Text<%>
  (Class #:implements Text:Basic<%>
         [start-colorer ((Symbol -> String)
                         (U (Input-Port
                             -> (Values Any
                                        Symbol
                                        (Option Symbol)
                                        (Option Positive-Integer)
                                        (Option Positive-Integer)))
                            (Input-Port
                             Positive-Integer
                             Any ; TODO (not/c dont-stop?)
                             -> (Values Any
                                        Symbol
                                        (Option Symbol)
                                        (Option Positive-Integer)
                                        (Option Positive-Integer)
                                        Positive-Integer
                                        Any)))
                         (Listof (List Symbol Symbol))
                         -> Void)]
         [stop-colorer ([] [Boolean] . ->* . Void)]
         [force-stop-colorer (Boolean -> Void)]
         [is-stopped? (-> Boolean)]
         [is-frozen? (-> Boolean)]
         [freeze-colorer (-> Void)]
         [thaw-colorer ([] [Boolean Boolean] . ->* . Void)]
         [reset-region (Natural (U Natural 'end) -> Void)]
         [reset-regions ((Listof (List Natural (U Natural 'end))) -> Void)]
         [get-spell-check-strings (-> Boolean)]
         [set-spell-check-strings (Boolean -> Void)]
         [get-spell-check-text (-> Boolean)]
         [set-spell-check-text (Boolean -> Void)]
         [set-spell-current-dict ((Option String) -> Void)]
         [get-spell-current-dict (-> (Option String))]
         [get-spell-suggestions (Natural -> (Option (List Natural Natural (Listof String))))]
         [get-regions (-> (Listof (List Natural (U Natural 'end))))]
         [skip-whitespace (Natural (U 'forward 'backward) Boolean -> Natural)]
         [backward-match (Natural Natural -> (U Natural))]
         [backward-containing-sexp (Natural Natural -> (Option Natural))]
         [forward-match (Natural Natural -> (Option Natural))]
         [insert-close-paren ([Natural Char Boolean Boolean]
                              [(U #f 'adjacent 'forward)]
                              . ->* . Void)]
         [classify-position (Natural -> (Option Symbol))]
         [get-token-range (Natural -> (Values (Option Natural) (Option Natural)))]
         [on-lexer-valid (Boolean -> Any)]
         [is-lexer-valid? (-> Boolean)]))

(define-type Color:Text-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text:Basic<%>)
    ->
    (Class #:row-var r #:implements Color:Text<%>)))

(define-type Color:Text%
  (Class #:implements Color:Text<%>))

(define-type Color:Text-Mode<%> (Class))

(define-type Color:Text-Mode-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Mode:Surrogate-Text<%>)
    ->
    (Class #:row-var r #:implements Color:Text-Mode<%>)))

(define-type Color:Text-Mode%
  (Class #:implements Color:Text-Mode<%>))

;; 8 Comment Box
(provide Comment-Box:Snip%)
(define-type Comment-Box:Snip%
  ;; TODO: refine return type?
  (Class #:implements Editor-Snip:Decorated%
         #:implements Readable-Snip<%>))

;; 10 Editor Snip
(provide Editor-Snip:Decorated<%>
         Editor-Snip:Decorated%
         Editor-Snip:Decorated-Mixin
         Editor-Snip:Decorated-Snipclass%)

(define-type Editor-Snip:Decorated<%>
  (Class #:implements Editor-Snip%
         [get-corner-bitmap (-> (Option (Instance Bitmap%)))]
         [get-color (-> (U String Color%-Instance))]
         [get-menu (-> (Option (Instance Popup-Menu%)))]
         [get-position (-> (U 'top-right 'left-top))]
         [reset-min-sizes (-> Void)]))

(define-type Editor-Snip:Decorated-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Snip%)
    ->
    (Class #:row-var r #:implements Editor-Snip:Decorated<%>
           [get-corner-bitmap (-> #f)]
           [get-color (-> (U "white" "black"))]
           [get-menu (-> #f)]
           [get-position (-> 'top-right)])))

(define-type Editor-Snip:Decorated%
  (Class #:implements Editor-Snip:Decorated<%>
         [make-snip (-> (Instance Editor-Snip:Decorated%))]
         [make-editor (-> Editor<%>-Instance)]
         [copy (-> (Instance Editor-Snip:Decorated<%> #|FIXME Editor-Snip:Decorated%|#))]))

(define-type Editor-Snip:Decorated-Snipclass%
  (Class #:implements Snip-Class%
         [make-snip ((Instance Editor-Stream-In%) -> (Instance Editor-Snip:Decorated<%>))]
         [read ((Instance Editor-Stream-In%) -> (Instance Editor-Snip:Decorated<%>))]))

;; 11 Editor
(provide Editor:Basic<%>
         Editor:Basic-Mixin
         Editor:Standard-Style-List<%>
         Editor:Standard-Style-List-Mixin
         Editor:Keymap<%>
         Editor:Keymap-Mixin
         Editor:Autowrap<%>
         Editor:Autowrap-Mixin
         Editor:File<%>
         Editor:File-Mixin
         Editor:Backup-Autosave<%>
         Editor:Backup-Autosave-Mixin
         Editor:Info<%>
         Editor:Info-Mixin)

(define-type Editor:Basic<%>
  (Class #:implements Editor<%>
         [has-focus? (-> Boolean)]
         [local-edit-sequence? (-> Boolean)]
         [run-after-edit-sequence ([(-> Void)]
                                   [(Option Symbol)]
                                   . ->* .
                                   Void)]
         [get-top-level-window (-> (Option (Instance Top-Level-Window<%>)))]
         [save-file-out-of-date? (-> Boolean)]
         [save-file/gui-error ([]
                               [(Option String)
                                (U 'guess 'standard 'text 'text-force-cr 'same 'copy)
                                Boolean]
                               . ->* .
                               Boolean)]
         [load-file/gui-error ([]
                               [(Option String)
                                (U 'guess 'standard 'text 'text-force-cr 'same 'copy)
                                Boolean]
                               . ->* .
                               Boolean)]
         [on-close (-> Void)]
         [can-close? (-> Boolean)]
         [close (-> Boolean)]
         [get-filename/untitled-name (-> String)]
         [get-pos/text (Mouse-Event%-Instance -> (Values (Option Natural)
                                                         (Option Editor<%>-Instance)))]
         [get-pos/text-dc-location (Integer Integer -> (Values (Option Natural)
                                                               (Option Editor<%>-Instance)))]))

(define-type Editor:Basic-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Editor<%>)
    ->
    (Class #:row-var r #:implements Editor:Basic<%>)))

(define-type Editor:Standard-Style-List<%>
  (Class #:implements Editor<%>))

(define-type Editor:Standard-Style-List-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Editor<%>)
    ->
    (Class #:row-var r #:implements Editor:Standard-Style-List<%>)))

(define-type Editor:Keymap<%>
  (Class #:implements Editor:Basic<%>
         [get-keymaps (-> (Listof (Instance Keymap%)))]))

(define-type Editor:Keymap-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Editor:Basic<%>)
    ->
    (Class #:row-var r #:implements Editor:Keymap<%>)))

(define-type Editor:Autowrap<%>
  (Class #:implements Editor:Basic<%>))

(define-type Editor:Autowrap-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Editor:Basic<%>)
    ->
    (Class #:row-var r #:implements Editor:Autowrap<%>)))

(define-type Editor:File<%>
  (Class #:implements Editor:Keymap<%>
         [get-can-close-parent (-> (U #f (Instance Frame%) Dialog%-Instance))]
         [update-frame-filename (-> Void)]
         [allow-close-with-no-filename? (-> Boolean)]
         [user-saves-or-not-modified? (#t -> Boolean)] ; FIXME: fishy docs
         ))

(define-type Editor:File-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Editor:Keymap<%>)
    ->
    (Class #:row-var r #:implements Editor:File<%>)))

(define-type Editor:Backup-Autosave<%>
  (Class #:implements Editor:Basic<%>
         [backup? (-> Boolean)]
         [autosave? (-> Boolean)]
         [do-autosave (-> (Option Path))]
         [remove-autosave (-> Void)]))

(define-type Editor:Backup-Autosave-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Editor:Basic<%>)
    ->
    (Class #:row-var r
           #:implements Editor:Backup-Autosave<%>
           #:implements Autosave:Autosavable<%>)))

(define-type Editor:Info<%>
  (Class #:implements Editor:Basic<%>))

(define-type Editor:Info-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Editor:Basic<%>)
    ->
    (Class #:row-var r #:implements Editor:Info<%>)))

;; 14 Frame
(provide Frame:Basic<%>
         Frame:Basic%
         Frame:Basic-Mixin
         Frame:Focus-Table<%>
         Frame:Focus-Table-Mixin
         Frame:Size-Pref<%>
         Frame:Size-Pref%
         Frame:Size-Pref-Mixin
         Frame:Register-Group<%>
         Frame:Register-Group-Mixin
         Frame:Status-Line<%>
         Frame:Status-Line%
         Frame:Status-Line-Mixin
         Frame:Info<%>
         Frame:Info%
         Frame:Info-Mixin
         Frame:Text-Info<%>
         Frame:Text-Info%
         Frame:Text-Info-Mixin
         Frame:Pasteboard-Info<%>
         Frame:Pasteboard-Info%
         Frame:Pasteboard-Info-Mixin
         Frame:Standard-Menus<%>
         Frame:Standard-Menus%
         Frame:Standard-Menus-Mixin
         Frame:Editor<%>
         Frame:Editor%
         Frame:Editor-Mixin
         Frame:Text<%>
         Frame:Text%
         Frame:Text-Mixin
         Frame:Pasteboard<%>
         Frame:Pasteboard%
         Frame:Pasteboard-Mixin
         Frame:Delegate<%>
         Frame:Delegate%
         Frame:Delegate-Mixin
         Frame:Searchable<%>
         Frame:Searchable%
         Frame:Searchable-Mixin
         Frame:Searchable-Text<%>
         Frame:Searchable-Text-Mixin)

(define-type Frame:Basic<%>
  (Class #:implements Frame%
         [get-area-container% (-> Area-Container<%> #|FIXME (implementation?/c area-container<%>)|#)]
         [get-area-container (-> Area-Container<%>-Instance)]
         [get-menu-bar% (-> ClassTop #|FIXME (subclass?/c menu-bar%)|#)]
         [make-root-area-container
          (Area-Container<%> #|FIXME (implementation?/c area-container<%>)|#
           Area-Container<%>-Instance
           -> Area-Container<%>-Instance)]
         [close (-> Void)]
         [editing-this-file? (Path -> Boolean)]
         [get-filename ([] [(Option (Boxof Boolean))] . ->* . (Option Path))]
         [make-visible (String -> Void)]))

(define-type Frame:Basic-Mixin
  (All (r #:row)
       (Class #:row-var r #:implements Frame%)
       ->
       (Class #:row-var r #:implements Frame:Basic<%>)))

(define-type Frame:Focus-Table<%>
  (Class #:implements Frame%))

(define-type Frame:Focus-Table-Mixin
  (All (r #:row)
       (Class #:row-var r #:implements Frame%)
       ->
       (Class #:row-var r #:implements Frame:Focus-Table<%>)))

(define-type Frame:Size-Pref<%>
  (Class #:implements Frame:Basic<%>
         [adjust-size-when-monitor-setup-changes? (-> Boolean)]))

(define-type Frame:Size-Pref-Mixin
 (All (r #:row)
      (Class #:row-var r #:implements Frame%)
      ->
      (Class #:row-var r #:implements Frame:Size-Pref<%>
             (init [size-preferences Symbol]
                   [position-preferences-key (Option Symbol) #:optional]
                   [width (Option Natural)]
                   [height (Option Natural)]
                   [x (Option Integer)]
                   [y (Option Integer)]))))

(define-type Frame:Register-Group<%>
  (Class #:implements Frame%))

(define-type Frame:Register-Group-Mixin
  (All (r #:row)
       (Class #:row-var r #:implements Frame:Basic<%>)
       ->
       (Class #:row-var r #:implements Frame:Register-Group<%>)))

(define-type Frame:Status-Line<%>
  ;; Note: if you change this next line to
  ;; #:implements Frame%, then the mixin using this
  ;; type below should be ruled out by sealing contracts.
  ;;
  ;; TODO: implement sealing contracts and make sure
  ;; that mistake is ruled out
  (Class #:implements Frame:Basic<%>
         [open-status-line (Symbol -> Void)]
         [close-status-line (Symbol -> Void)]
         [update-status-line
          (Symbol (Option String) -> Void)]))

(define-type Frame:Status-Line-Mixin
  (All (r #:row)
       (Class #:row-var r #:implements Frame:Basic<%>)
       ->
       (Class #:row-var r #:implements Frame:Status-Line<%>)))

(define-type Frame:Info<%>
  (Class #:implements Frame:Basic<%>
         [determine-width
          (String Editor-Canvas%-Instance Text%-Instance -> Integer)]
         [lock-status-changed (-> Void)]
         [udpate-info (-> Void)]
         [set-info-canvas ((Option (Instance Canvas:Basic%)) -> Void)]
         [get-info-canvas (-> (Option (Instance Canvas:Basic%)))]
         [get-info-editor (-> (Option Editor<%>-Instance))]
         [get-info-panel (-> (Instance Horizontal-Panel%))]
         [show-info (-> Void)]
         [hide-info (-> Void)]
         [is-info-hidden? (-> Boolean)]))

(define-type Frame:Info-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Frame:Basic<%>)
    ->
    (Class #:row-var r #:implements Frame:Info<%>)))

(define-type Frame:Text-Info<%>
  (Class #:implements Frame:Info<%>
         [set-macro-recording (Boolean -> Void)]
         [overwrite-status-changed (-> Void)]
         [anchor-status-changed (-> Void)]
         [editor-position-changed (-> Void)]
         [add-line-number-menu-items ((Instance Menu-Item-Container<%>) -> Void)]))

(define-type Frame:Text-Info-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Frame:Info<%>)
    ->
    (Class #:row-var r #:implements Frame:Text-Info<%>)))

(define-type Frame:Pasteboard-Info<%>
  (Class #:implements Frame:Info<%>))

(define-type Frame:Pasteboard-Info-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Frame:Basic<%>)
    ->
    (Class #:row-var r #:implements Frame:Pasteboard-Info<%>)))

(define-type Frame:Standard-Menus<%>
  (Class #:implements Frame:Basic<%>
         [on-close (-> Void)]
         [get-menu% (-> (Instance Menu:Can-Restore-Underscore-Menu%))]
         [get-menu-item% (-> (Instance Menu:Can-Restore-Menu-Item%))]
         [get-checkable-menu-item%
          (-> (Instance Menu:Can-Restore-Checkable-Menu-Item%))]
         [get-file-menu (-> Menu%-Instance)]
         [get-edit-menu (-> Menu%-Instance)]
         [get-help-menu (-> Menu%-Instance)]
         [file-menu:get-new-item (-> (Option Menu-Item%-Instance))]
         [file-menu:create-new? (-> Boolean)]
         [file-menu:new-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [file-menu:new-on-demand (Menu-Item%-Instance -> Void)]
         [file-menu:new-string (-> String)]
         [file-menu:new-help-string (-> String)]
         [file-menu:between-new-and-open (Menu-Item%-Instance -> Void)]
         [file-menu:get-open-item (-> (Option Menu-Item%-Instance))]
         [file-menu:create-open? (-> Boolean)]
         [file-menu:open-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [file-menu:open-on-demand (Menu-Item%-Instance -> Void)]
         [file-menu:open-string (-> String)]
         [file-menu:open-help-string (-> String)]
         [file-menu:get-open-recent-item (-> (Option Menu-Item%-Instance))]
         [file-menu:create-open-recent? (-> Boolean)]
         [file-menu:open-recent-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [file-menu:open-recent-on-demand (Menu-Item%-Instance -> Void)]
         [file-menu:open-recent-string (-> String)]
         [file-menu:open-recent-help-string (-> String)]
         [file-menu:between-open-and-revert (Menu-Item%-Instance -> Void)]
         [file-menu:get-revert-item (-> (Option Menu-Item%-Instance))]
         [file-menu:create-revert? (-> Boolean)]
         [file-menu:revert-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [file-menu:revert-on-demand (Menu-Item%-Instance -> Void)]
         [file-menu:revert-string (-> String)]
         [file-menu:revert-help-string (-> String)]
         [file-menu:between-revert-and-save (Menu-Item%-Instance -> Void)]
         [file-menu:get-save-item (-> (Option Menu-Item%-Instance))]
         [file-menu:create-save? (-> Boolean)]
         [file-menu:save-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [file-menu:save-on-demand (Menu-Item%-Instance -> Void)]
         [file-menu:save-string (-> String)]
         [file-menu:save-help-string (-> String)]
         [file-menu:get-save-as-item (-> (Option Menu-Item%-Instance))]
         [file-menu:create-save-as? (-> Boolean)]
         [file-menu:save-as-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [file-menu:save-as-on-demand (Menu-Item%-Instance -> Void)]
         [file-menu:save-as-string (-> String)]
         [file-menu:save-as-help-string (-> String)]
         [file-menu:between-save-as-and-print (Menu-Item%-Instance -> Void)]
         [file-menu:get-print-item (-> (Option Menu-Item%-Instance))]
         [file-menu:create-print? (-> Boolean)]
         [file-menu:print-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [file-menu:print-on-demand (Menu-Item%-Instance -> Void)]
         [file-menu:print-string (-> String)]
         [file-menu:print-help-string (-> String)]
         [file-menu:between-print-and-close (Menu-Item%-Instance -> Void)]
         [file-menu:get-close-item (-> (Option Menu-Item%-Instance))]
         [file-menu:create-close? (-> Boolean)]
         [file-menu:close-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [file-menu:close-on-demand (Menu-Item%-Instance -> Void)]
         [file-menu:close-string (-> String)]
         [file-menu:close-help-string (-> String)]
         [file-menu:between-close-and-quit (Menu-Item%-Instance -> Void)]
         [file-menu:get-quit-item (-> (Option Menu-Item%-Instance))]
         [file-menu:create-quit? (-> Boolean)]
         [file-menu:quit-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [file-menu:quit-on-demand (Menu-Item%-Instance -> Void)]
         [file-menu:quit-string (-> String)]
         [file-menu:quit-help-string (-> String)]
         [file-menu:after-quit (Menu-Item%-Instance -> Void)]
         [edit-menu:get-undo-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-undo? (-> Boolean)]
         [edit-menu:undo-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:undo-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:undo-string (-> String)]
         [edit-menu:undo-help-string (-> String)]
         [edit-menu:get-redo-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-redo? (-> Boolean)]
         [edit-menu:redo-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:redo-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:redo-string (-> String)]
         [edit-menu:redo-help-string (-> String)]
         [edit-menu:between-redo-and-cut (Menu-Item%-Instance -> Void)]
         [edit-menu:get-cut-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-cut? (-> Boolean)]
         [edit-menu:cut-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:cut-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:cut-string (-> String)]
         [edit-menu:cut-help-string (-> String)]
         [edit-menu:between-cut-and-copy (Menu-Item%-Instance -> Void)]
         [edit-menu:get-copy-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-copy? (-> Boolean)]
         [edit-menu:copy-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:copy-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:copy-string (-> String)]
         [edit-menu:copy-help-string (-> String)]
         [edit-menu:between-copy-and-paste (Menu-Item%-Instance -> Void)]
         [edit-menu:get-paste-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-paste? (-> Boolean)]
         [edit-menu:paste-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:paste-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:paste-string (-> String)]
         [edit-menu:paste-help-string (-> String)]
         [edit-menu:between-paste-and-clear (Menu-Item%-Instance -> Void)]
         [edit-menu:get-clear-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-clear? (-> Boolean)]
         [edit-menu:clear-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:clear-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:clear-string (-> String)]
         [edit-menu:clear-help-string (-> String)]
         [edit-menu:between-clear-and-select-all (Menu-Item%-Instance -> Void)]
         [edit-menu:get-select-all-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-select-all? (-> Boolean)]
         [edit-menu:select-all-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:select-all-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:select-all-string (-> String)]
         [edit-menu:select-all-help-string (-> String)]
         [edit-menu:between-select-all-and-find (Menu-Item%-Instance -> Void)]
         [edit-menu:get-find-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-find? (-> Boolean)]
         [edit-menu:find-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:find-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:find-string (-> String)]
         [edit-menu:find-help-string (-> String)]
         [edit-menu:get-find-from-selection-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-find-from-selection? (-> Boolean)]
         [edit-menu:find-from-selection-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:find-from-selection-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:find-from-selection-string (-> String)]
         [edit-menu:find-from-selection-help-string (-> String)]
         [edit-menu:get-find-next-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-find-next? (-> Boolean)]
         [edit-menu:find-next-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:find-next-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:find-next-string (-> String)]
         [edit-menu:find-next-help-string (-> String)]
         [edit-menu:get-find-previous-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-find-previous? (-> Boolean)]
         [edit-menu:find-previous-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:find-previous-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:find-previous-string (-> String)]
         [edit-menu:find-previous-help-string (-> String)]
         [edit-menu:get-show/hide-replace-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-show/hide-replace? (-> Boolean)]
         [edit-menu:show/hide-replace-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:show/hide-replace-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:show/hide-replace-string (-> String)]
         [edit-menu:show/hide-replace-help-string (-> String)]
         [edit-menu:get-replace-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-replace? (-> Boolean)]
         [edit-menu:replace-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:replace-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:replace-string (-> String)]
         [edit-menu:replace-help-string (-> String)]
         [edit-menu:get-replace-all-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-replace-all? (-> Boolean)]
         [edit-menu:replace-all-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:replace-all-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:replace-all-string (-> String)]
         [edit-menu:replace-all-help-string (-> String)]
         [edit-menu:get-find-case-sensitive-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-find-case-sensitive? (-> Boolean)]
         [edit-menu:find-case-sensitive-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:find-case-sensitive-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:find-case-sensitive-string (-> String)]
         [edit-menu:find-case-sensitive-help-string (-> String)]
         [edit-menu:between-find-and-preferences (Menu-Item%-Instance -> Void)]
         [edit-menu:get-preferences-item (-> (Option Menu-Item%-Instance))]
         [edit-menu:create-preferences? (-> Boolean)]
         [edit-menu:preferences-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [edit-menu:preferences-on-demand (Menu-Item%-Instance -> Void)]
         [edit-menu:preferences-string (-> String)]
         [edit-menu:preferences-help-string (-> String)]
         [edit-menu:after-preferences (Menu-Item%-Instance -> Void)]
         [help-menu:before-about (Menu-Item%-Instance -> Void)]
         [help-menu:get-about-item (-> (Option Menu-Item%-Instance))]
         [help-menu:create-about? (-> Boolean)]
         [help-menu:about-callback
          (Menu-Item%-Instance Control-Event%-Instance -> Void)]
         [help-menu:about-on-demand (Menu-Item%-Instance -> Void)]
         [help-menu:about-string (-> String)]
         [help-menu:about-help-string (-> String)]
         [help-menu:after-about (Menu-Item%-Instance -> Void)]))

(define-type Frame:Standard-Menus-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Frame:Basic<%>)
    ->
    (Class #:row-var r #:implements Frame:Standard-Menus<%>)))

(define-type Frame:Editor<%>
  (Class #:implements Frame:Standard-Menus<%>
         [get-entire-label (-> String)]
         [get-label-prefix (-> String)]
         [set-label-prefix (String -> Void)]
         [get-canvas% (-> ClassTop #|FIXME (subclass?/c editor-canvas%)|#)]
         [get-canvas<%> (-> (Instance Canvas:Basic%))]
         [get-editor% (-> Editor<%> #|FIXME (implementation?/c editor<%>)|#)]
         [get-editor<%> (-> Any #|FIXME interface?|#)]
         [make-editor (-> Editor<%>-Instance)]
         [revert (-> Void)]
         [save ([]
                [(U 'guess 'standard 'text 'text-force-cr 'same 'copy)]
                . ->* . Boolean)]
         [save-as ([]
                   [(U 'guess 'standard 'text 'text-force-cr 'same 'copy)]
                   . ->* . Boolean)]
         [get-canvas (-> Canvas%-Instance)]
         [get-editor (-> Editor<%>-Instance)]))

(define-type Frame:Editor-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Frame:Standard-Menus<%>)
    ->
    (Class #:row-var r #:implements Frame:Editor<%>
           (init [filename String]
                 [editor% Editor:Basic<%> #|FIXME (implementation?/c editor:basic<%>)|#]
                 [parent (Option (Instance Frame%)) #:optional]
                 [width (Option Natural)]
                 [height (Option Natural)]
                 [x (Option Integer)]
                 [y (Option Integer)]
                 [style (Listof (U 'no-resize-border 'no-caption
                                   'no-system-menu 'hide-menu-bar
                                   'mdi-parent 'mdi-child
                                   'toolbar-button 'float 'metal))]
                 [enabled Any]
                 [border Natural]
                 [spacing Natural]
                 [alignment (List (U 'left 'center 'right)
                                  (U 'top 'center 'bottom))]
                 [min-width Natural]
                 [min-height Natural]
                 [stretchable-width Any]
                 [stretchable-height Any]))))

(define-type Frame:Text<%>
  (Class #:implements Frame:Editor<%>))

(define-type Frame:Text-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Frame:Editor<%>)
    ->
    (Class #:row-var r #:implements Frame:Text<%>
           (init [editor% Text% #|FIXME (extends text%)|#]))))

(define-type Frame:Pasteboard<%>
  (Class #:implements Frame:Editor<%>))

(define-type Frame:Pasteboard-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Frame:Editor<%>)
    ->
    (Class #:row-var r #:implements Frame:Pasteboard<%>
           (init [editor% Pasteboard% #|FIXME (extends pasteboard%)|#]))))

(define-type Frame:Delegate<%>
  (Class #:implements Frame:Status-Line<%>
         #:implements Frame:Text<%>
         [get-delegated-text (-> (Option (Instance Text:Delegate<%>)))]
         [set-delegated-text ((Option (Instance Text:Delegate<%>)) -> Void)]
         [delegated-text-shown? (-> Boolean)]
         [hide-delegated-text (-> Void)]
         [show-delegated-text (-> Void)]
         [delegate-moved (-> Void)]))

(define-type Frame:Delegate-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Frame:Status-Line<%>
           #:implements Frame:Text<%>)
    ->
    (Class #:row-var r #:implements Frame:Delegate<%>)))

(define-type Frame:Searchable<%>
  (Class #:implements Frame:Basic<%>
         [search ((U 'forward 'backward) -> Void)]
         [search-replace (-> Boolean)]
         [replace-all (-> Void)]
         [get-text-to-search (-> Text%-Instance)]
         [set-text-to-search
          ((Option (Instance Text% #|FIXME (subclass?/c text%)|#)) -> Void)]
         [search-hidden? (-> Boolean)]
         [hide-search (-> Void)]
         [unhide-search ([Boolean]
                         [#:new-search-string-from-selection? Boolean]
                         . ->* . Void)]
         [unhide-search-and-toggle-focus ([]
                                          [#:new-search-string-from-selection? Boolean]
                                          . ->* . Void)]
         [get-case-sensitive-search? (-> Boolean)]
         [search-hits-changed (-> Void)]))

(define-type Frame:Searchable-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Frame:Standard-Menus<%>)
    ->
    (Class #:row-var r #:implements Frame:Searchable<%>
           [edit-menu:find-callback (-> Boolean)]
           [edit-menu:create-find? (-> #t)]
           ; FIXME: doc says "Overrides <method not found>" from here on
           [edit-menu:find-again-callback
            (Menu-Item%-Instance Control-Event%-Instance -> Void)]
           [edit-menu:create-find-again? (-> #t)]
           [edit-menu:find-again-backwards-callback
            (Menu-Item%-Instance Control-Event%-Instance -> Void)]
           [edit-menu:create-find-again-backwards? (-> #t)]
           ; end bug doc saying "Overrides <method not found>"
           [edit-menu:create-replace-all? (-> #t)]
           [edit-menu:create-find-case-sensitive? (-> #t)])))

(define-type Frame:Searchable-Text<%>
  (Class #:implements Frame:Searchable<%>
         #:implements Frame:Text<%>))

(define-type Frame:Searchable-Text-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Frame:Text<%>
           #:implements Frame:Searchable<%>)
    ->
    (Class #:row-var r #:implements Frame:Searchable-Text<%>)))

;; FIXME: not sure about types for these classes
(define-type Frame:Basic%
  (Class #:implements Frame:Register-Group<%>))
(define-type Frame:Size-Pref%
  (Class #:implements Frame:Size-Pref<%>))
(define-type Frame:Info%
  (Class #:implements Frame:Info<%>))
(define-type Frame:Text-Info%
  (Class #:implements Frame:Text-Info<%>))
(define-type Frame:Pasteboard-Info%
  (Class #:implements Frame:Pasteboard-Info<%>))
(define-type Frame:Status-Line%
  (Class #:implements Frame:Status-Line<%>))
(define-type Frame:Standard-Menus%
  (Class #:implements Frame:Standard-Menus<%>))
(define-type Frame:Editor%
  (Class #:implements Frame:Editor<%>))
(define-type Frame:Text%
  (Class #:implements Frame:Text<%>))
(define-type Frame:Searchable%
  (Class #:implements Frame:Searchable<%>))
(define-type Frame:Delegate%
  (Class #:implements Frame:Delegate<%>))
(define-type Frame:Pasteboard%
  (Class #:implements Frame:Pasteboard<%>))

;; 15 Group
(provide Group:%)

(define-type Group:%
  (Class [get-mdi-parent (-> (Option (Instance Frame%)))]
         [get-frames (-> (Listof Frame:Basic<%>-Instance))]
         [frame-label-changed (Frame:Basic<%>-Instance -> Void)]
         [frame-shown/hidden (-> Void)]
         [for-each-frame ((Frame:Basic<%>-Instance -> Void) -> Void)]
         [get-active-frame (-> Frame:Basic<%>-Instance)]
         [set-active-frame (Frame:Basic<%>-Instance -> Void)]
         [insert-frame (Frame:Basic<%>-Instance -> Void)]
         [remove-frame (Frame:Basic<%>-Instance -> Void)]
         [clear (-> Boolean)]
         [on-close-all (-> Void)]
         [can-close-all? (-> Boolean)]
         [locate-file ((Option Frame:Basic<%>-Instance) -> Path)]))

;; 19 Keymap
(provide Keymap:Aug-Keymap<%>
         Keymap:Aug-Keymap%
         Keymap:Aug-Keymap-Mixin)

(define-type Keymap:Aug-Keymap<%>
  (Class #:implements Keymap%
         [get-chained-keymaps (-> (Listof (Instance Keymap%)))]
         [get-map-function-table (-> (HashTable String String))]
         [get-map-function-table/ht ((HashTable String String) -> (HashTable String String))]))

(define-type Keymap:Aug-Keymap-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Keymap%)
    ->
    (Class #:row-var r #:implements Keymap:Aug-Keymap<%>)))

(define-type Keymap:Aug-Keymap%
  (Class #:implements Keymap:Aug-Keymap<%>))

;; 20 Menu
(provide Menu:Can-Restore<%>
         Menu:Can-Restore-Mixin
         Menu:Can-Restore-Underscore<%>
         Menu:Can-Restore-Underscore-Mixin
         Menu:Can-Restore-Menu-Item%
         Menu:Can-Restore-Checkable-Menu-Item%
         Menu:Can-Restore-Underscore-Menu%)

(define-type Menu:Can-Restore<%>
  (Class #:implements Selectable-Menu-Item<%>
         [restore-keybinding (-> Void)]))

(define-type Menu:Can-Restore-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Selectable-Menu-Item<%>)
    ->
    (Class #:row-var r #:implements Menu:Can-Restore<%>)))

(define-type Menu:Can-Restore-Underscore<%>
  (Class #:implements Labelled-Menu-Item<%>
         [erase-underscores (-> Void)]
         [restore-underscores (-> Void)]))

(define-type Menu:Can-Restore-Underscore-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Labelled-Menu-Item<%>)
    ->
    (Class #:row-var r #:implements Menu:Can-Restore-Underscore<%>)))

;; FIXME not sure about types for classes
(define-type Menu:Can-Restore-Menu-Item%
  (Class #:implements Menu:Can-Restore<%>))
(define-type Menu:Can-Restore-Checkable-Menu-Item%
  (Class #:implements Menu:Can-Restore<%>))
(define-type Menu:Can-Restore-Underscore-Menu%
  (Class #:implements Menu:Can-Restore<%>))

;; 21 Mode
(provide Mode:Surrogate-Text<%>
         Mode:Surrogate-Text%
         Mode:Host-Text<%>
         Mode:Host-Text-Mixin)

(define-type File-Format (U 'guess 'same 'copy 'standard 'text 'text-force-cr))
(define-type Image-Kind (U 'unknown 'unknown/mask 'unknown/alpha
                           'gif 'gif/mask 'gif/alpha
                           'jpeg 'png 'png/mask 'png/alpha
                           'xbm 'xpm 'bmp 'pict))
(define-type Draw-Caret (U 'no-caret 'show-inactive-caret 'show-caret (Pairof Natural Natural)))
(define-type Edit-Op (U 'undo 'redo 'clear 'cut 'copy 'paste
                        'kill 'select-all 'insert-text-box
                        'insert-pasteboard-box 'insert-image))

(define-type Mode:Surrogate-Text<%>
  (Class [on-enable-surrogate (Text%-Instance -> Any)]
         [on-disable-surrogate (Text%-Instance -> Any)]))

(define-type Mode:Surrogate-Text%
  (Class #:implements Mode:Surrogate-Text<%>
         [on-change (Text%-Instance (-> Void) -> Void)]
         [on-char (Text%-Instance (-> Void) Key-Event%-Instance -> Void)]
         [on-default-char (Text%-Instance (-> Void) Key-Event%-Instance -> Void)]
         [on-default-client (Text%-Instance (-> Any) Any -> Any)]
         [on-display-size (Text%-Instance (-> Void) -> Void)]
         [on-edit-sequence (Text%-Instance (-> Void) -> Void)]
         [on-event (Text%-Instance (-> Void) Mouse-Event%-Instance -> Void)]
         [on-focus (Text%-Instance (-> Void) Any -> Void)]
         [on-load-file (Text%-Instance (-> Void) Path File-Format -> Void)]
         [on-local-char (Text%-Instance (-> Void) Key-Event%-Instance -> Void)]
         [on-local-event (Text%-Instance (-> Void) Mouse-Event%-Instance -> Void)]
         [on-new-box (Text%-Instance (-> Snip%-Instance) (U 'text 'pasteboard) -> Snip%-Instance)]
         #;[on-new-image-snip ; TODO: reeanable when Image-Snip% is available
          (Text%-Instance (-> (Instance Image-Snip%)) Path Image-Kind Any Any -> (Instance Image-Snip%))]
         [on-paint
          (Text%-Instance (-> Void) Any (Instance DC<%>) Real Real Real Real Real Real Draw-Caret -> Void)]
         [on-save-file (Text%-Instance (-> Void) Path File-Format -> Void)]
         [on-snip-modified (Text%-Instance (-> Void) Snip%-Instance Any -> Void)]
         [on-change-style (Text%-Instance (-> Any) Natural Natural -> Any)]
         [on-delete (Text%-Instance (-> Void) Natural Natural -> Void)]
         [on-insert (Text%-Instance (-> Void) Natural Natural -> Void)]
         [on-new-string-snip
          (Text%-Instance (-> (Instance String-Snip%)) -> (Instance String-Snip%))]
         [on-new-tab-snip (Text%-Instance (-> (Instance Tab-Snip%)) -> (Instance Tab-Snip%))]
         [on-set-size-constraint (Text%-Instance (-> Void) -> Void)]
         [after-change-style (Text%-Instance (-> Void) Natural Natural -> Void)]
         [after-delete (Text%-Instance (-> Void) Natural Natural -> Void)]
         [after-insert (Text%-Instance (-> Void) Natural Natural -> Void)]
         [after-set-position (Text%-Instance (-> Void) -> Void)]
         [after-set-size-constraint (Text%-Instance (-> Void) -> Void)]
         [after-edit-sequence (Text%-Instance (-> Void) -> Void)]
         [after-load-file (Text%-Instance (-> Void) Any -> Void)]
         [after-save-file (Text%-Instance (-> Void) Any -> Void)]
         [can-change-style? (Text%-Instance (-> Boolean) Natural Natural -> Boolean)]
         [can-delete? (Text%-Instance (-> Boolean) Natural Natural -> Boolean)]
         [can-insert? (Text%-Instance (-> Boolean) Natural Natural -> Boolean)]
         [can-set-size-constraint? (Text%-Instance (-> Boolean) -> Boolean)]
         [can-do-edit-operation? ([Text%-Instance (-> Boolean) Edit-Op]
                                  [Any]
                                  . ->* . Boolean)]
         [can-load-file? (Text%-Instance (-> Boolean) Path File-Format -> Boolean)]
         [can-save-file? (Text%-Instance (-> Boolean) Path File-Format -> Boolean)]
         [put-file
          (Text%-Instance
           (-> (Option Path-String))
           (Option Path-String) (Option Path-String)
           -> (Option Path-String))]))

(define-type Mode:Host-Text<%>
  (Class [get-surrogate (-> (Option (Instance Mode:Surrogate-Text<%>)))]
         [set-surrogate ((Option (Instance Mode:Surrogate-Text<%>)) -> Void)]))

(define-type Mode:Host-Text-Mixin
  (All (r #:row)
    (Class #:row-var r)
    ->
    (Class #:row-var r #:implements Mode:Host-Text<%>)))

;; 22 Number Snip
(provide Number-Snip:Snip-Class%)

(define-type Number-Snip:Snip-Class%
  (Class #:implements Snip-Class%))

;; 23 Panel
(provide Panel:Single<%>
         Panel:Single%
         Panel:Single-Mixin
         Panel:Single-Window<%>
         Panel:Single-Window-Mixin
         Panel:Single-Pane%
         Panel:Dragable<%>
         Panel:Dragable-Mixin
         Panel:Vertical-Dragable<%>
         Panel:Vertical-Dragable%
         Panel:Vertical-Dragable-Mixin
         Panel:Horizontal-Dragable<%>
         Panel:Horizontal-Dragable%
         Panel:Horizontal-Dragable-Mixin
         Panel:Splitter<%>
         Panel:Splitter-Mixin
         Panel:Discrete-Sizes<%>
         Panel:Discrete-Sizes-Mixin
         Panel:Horizontal-Discrete-Sizes%
         Panel:Vertical-Discrete-Sizes%
         Panel:Discrete-Child<%>)

(define-type Panel:Single<%>
  (Class #:implements Area-Container<%>
         [active-child (case->
                        [(Instance Area<%>) -> Void]
                        [-> (Instance Area<%>)])]))

(define-type Panel:Single-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Area-Container<%>)
    ->
    (Class #:row-var r #:implements Panel:Single<%>)))

(define-type Panel:Single-Window<%>
  (Class #:implements Panel:Single<%>
         #:implements Window<%>))

(define-type Panel:Single-Window-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Panel:Single<%>
           #:implements Window<%>)
    ->
    (Class #:row-var r #:implements Panel:Single-Window<%>)))

(define-type Panel:Single%
  (Class #:implements Panel:Single<%>))

(define-type Panel:Single-Pane%
  (Class #:implements Panel:Single<%>))

(define-type Panel:Dragable<%>
  (Class #:implements Window<%>
         #:implements Area-Container<%>
         [after-percentage-change (-> Void)]
         [get-default-percentages
          (Positive-Integer -> (Listof Nonnegative-Real #|FIXME (between/c 0 1)|#))]
         [right-click-in-gap
          (Mouse-Event%-Instance (Instance Subarea<%>) (Instance Subarea<%>) -> Void)]
         [set-percentages ((Listof Real) -> Void)]
         [get-percentages (-> (Listof Real))]
         [get-vertical? (-> Boolean)]
         [set-orientation (Boolean -> Void)]))

(define-type Panel:Vertical-Dragable<%>
  (Class #:implements Panel:Dragable<%>))

(define-type Panel:Horizontal-Dragable<%>
  (Class #:implements Panel:Dragable<%>))

(define-type Panel:Dragable-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Window<%>
           #:implements Area-Container<%>)
    ->
    (Class #:row-var r #:implements Panel:Dragable<%>)))

(define-type Panel:Vertical-Dragable-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Panel:Dragable<%>)
    ->
    (Class #:row-var r #:implements Panel:Vertical-Dragable<%>)))

(define-type Panel:Horizontal-Dragable-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Panel:Dragable<%>)
    ->
    (Class #:row-var r #:implements Panel:Horizontal-Dragable<%>)))

(define-type Panel:Vertical-Dragable%
  (Class #:implements Panel:Vertical-Dragable<%>))

(define-type Panel:Horizontal-Dragable%
  (Class #:implements Panel:Horizontal-Dragable<%>))

(define-type Panel:Splitter<%>
  (Class [split-vertical ((Instance Canvas<%>)
                          ((Instance Panel:Splitter<%>) -> (Instance Canvas<%>))
                          -> (Instance Canvas<%>))]
         [split-horizontal ((Instance Canvas<%>)
                            ((Instance Panel:Splitter<%>) -> (Instance Canvas<%>))
                            -> (Instance Canvas<%>))]
         [collapse ((Instance Canvas<%>) -> Void)]))

(define-type Panel:Splitter-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Area-Container<%>
           #:implements Panel:Dragable<%>)
    ->
    (Class #:row-var r #:implements Panel:Splitter<%>)))

(define-type Panel:Discrete-Sizes<%>
  (Class [set-orientation (Boolean -> Void)]
         [get-orientation (-> Boolean)]))

(define-type Panel:Discrete-Child<%>
  (Class [get-discrete-widths (-> (Listof Natural))]
         [get-discrete-heights (-> (Listof Natural))]))

(define-type Panel:Discrete-Sizes-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Panel%)
    ->
    (Class #:row-var r
           #:implements Panel:Discrete-Sizes<%>
           #:implements Panel:Discrete-Child<%>)))

(define-type Panel:Horizontal-Discrete-Sizes%
  (Class #:implements Panel:Discrete-Sizes<%>))

(define-type Panel:Vertical-Discrete-Sizes%
  (Class #:implements Panel:Discrete-Sizes<%>))

;; 24 Pasteboard
(provide Pasteboard:Basic%
         Pasteboard:Standard-Style-List%
         Pasteboard:Keymap%
         Pasteboard:File%
         Pasteboard:Backup-Autosave%
         Pasteboard:Info%)

(define-type Pasteboard:Basic%
  (Class #:implements Editor:Basic<%>))

(define-type Pasteboard:Standard-Style-List%
  (Class #:implements Editor:Standard-Style-List<%>))

(define-type Pasteboard:Keymap%
  (Class #:implements Editor:Keymap<%>))

(define-type Pasteboard:File%
  (Class #:implements Editor:File<%>))

(define-type Pasteboard:Backup-Autosave%
  (Class #:implements Editor:Backup-Autosave<%>))

(define-type Pasteboard:Info%
  (Class #:implements Editor:Info<%>))

;; 28 Racket
(provide Racket:Sexp-Snip<%>
         Racket:Sexp-Snip%
         Racket:Text<%>
         Racket:Text%
         Racket:Text-Mixin
         Racket:Text-Mode<%>
         Racket:Text-Mode%
         Racket:Text-Mode-Mixin
         Racket:Set-Mode-Mixin)

(define-type Racket:Sexp-Snip<%>
  (Class [get-saved-snips (-> (Listof Snip%-Instance))]))

(define-type Racket:Sexp-Snip%
  (Class #:implements Snip%
         #:implements Racket:Sexp-Snip<%>
         #:implements Readable-Snip<%>))

(define-type Racket:Text<%>
  (Class #:implements Text:Basic<%>
         #:implements Mode:Host-Text<%>
         #:implements Color:Text<%>
         [get-limit (-> Integer)]
         [balance-parens (Key-Event%-Instance -> Void)]
         [tabify-on-return? (-> Boolean)]
         [tabify ([] [Integer] . ->* . Void)]
         [tabify-selection ([] [Integer Integer] . ->* . Void)]
         [tabify-all (-> Void)]
         [insert-return (-> Void)]
         [box-comment-out-selection ((U 'start Integer) (U 'end Integer) -> Void)]
         [comment-out-selection (Integer Integer -> Void)]
         [uncomment-selection (Integer Integer -> Void)]
         [get-forward-sexp (Integer -> (Option Integer))]
         [remove-sexp (Integer -> Void)]
         [forward-sexp (Integer -> Void)]
         [flash-forward-sexp (Integer -> Void)]
         [get-backward-sexp (Integer -> (Option Integer))]
         [flash-backward-sexp (Integer -> Void)]
         [backward-sexp (Integer -> Void)]
         [find-up-sexp (Integer -> (Option Integer))]
         [up-sexp (Integer -> Void)]
         [find-down-sexp (Integer -> (Option Integer))]
         [down-sexp (Integer -> Void)]
         [remove-parens-forward (Integer -> Void)]
         [select-forward-sexp (-> Void)]
         [select-backward-sexp (-> Void)]
         [select-up-sexp (-> Void)]
         [select-down-sexp (-> Void)]
         [transpose-sexp (Integer -> Void)]
         [mark-matching-parenthesis (Positive-Integer -> Void)]
         [get-tab-size (-> Integer)]
         [set-tab-size (Integer -> Void)]
         [introduce-let-ans (Integer -> Void)]
         [move-sexp-out (Integer -> Void)]))

(define-type Racket:Text-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Text:Basic<%>
           #:implements Mode:Host-Text<%>
           #:implements Color:Text<%>
           #:implements Text:Autocomplete<%>)
    ->
    (Class #:row-var r #:implements Racket:Text<%>)))

(define-type Racket:Text-Mode<%> (Class))

(define-type Racket:Text-Mode-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Color:Text-Mode<%>
           #:implements Mode:Surrogate-Text<%>)
    ->
    (Class #:row-var r #:implements Racket:Text-Mode<%>)))

(define-type Racket:Set-Mode-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Racket:Text<%>
           #:implements Mode:Host-Text<%>)
    ->
    (Class #:row-var r #:implements Racket:Text-Mode<%>)))

(define-type Racket:Text%
  (Class #:implements Racket:Text-Mode<%>
         #:implements Racket:Text<%>
         #:implements Text:Autocomplete<%>
         #:implements Mode:Host-Text<%>))

(define-type Racket:Text-Mode%
  (Class #:implements Racket:Text-Mode<%>))

;; 29 Text
(provide Text:Basic<%>
         Text:Basic%
         Text:Basic-Mixin
         Text:Line-Spacing<%>
         Text:Line-Spacing%
         Text:Line-Spacing-Mixin
         Text:First-Line<%>
         Text:First-Line-Mixin
         Text:Foreground-Color<%>
         Text:Foreground-Color-Mixin
         Text:Hide-Caret/Selection<%>
         Text:Hide-Caret/Selection%
         Text:Hide-Caret/Selection-Mixin
         Text:Nbsp->Space<%>
         Text:Nbsp->Space%
         Text:Nbsp->Space-Mixin
         Text:Column-Guide<%>
         Text:Column-Guide-Mixin
         Text:Normalize-Paste<%>
         Text:Normalize-Paste%
         Text:Normalize-Paste-Mixin
         Text:Searching<%>
         Text:Searching%
         Text:Searching-Mixin
         Text:Return<%>
         Text:Return%
         Text:Return-Mixin
         Text:Wide-Snip<%>
         Text:Wide-Snip%
         Text:Wide-Snip-Mixin
         Text:Delegate<%>
         Text:Delegate%
         Text:Delegate-Mixin
         Text:Info<%>
         Text:Info%
         Text:Info-Mixin
         Text:Clever-File-Format<%>
         Text:Clever-File-Format%
         Text:Clever-File-Format-Mixin
         Text:Crlf-Line-Endings<%>
         Text:Crlf-Line-Endings-Mixin
         Text:File<%>
         Text:File%
         Text:File-Mixin
         Text:Ports<%>
         Text:Ports-Mixin
         Text:Input-Box<%>
         Text:Input-Box%
         Text:Input-Box-Mixin
         Text:Autocomplete<%>
         Text:Autocomplete-Mixin
         Text:Line-Numbers<%>
         Text:Line-Numbers-Mixin
         Text:1-Pixel-String-Snip%
         Text:1-Pixel-Tab-Snip%
         Text:Standard-Style-List%
         Text:Keymap%
         Text:Autowrap%
         Text:Backup-Autosave%)

(define-type Text:Basic<%>
  (Class #:implements Text%
         [highlight-range
          ([Natural Natural (U String Color%-Instance)]
           [Boolean
            (U 'high 'low)
            (U 'rectangle 'ellipse 'hollow-ellipse 'dot)
            #:adjust-on-insert/delete Boolean
            #:key Any]
           . ->* . ; FIXME: result type depends on `adjust-on-insert/delete`
           (U Void (-> Void)))]
         [unhighlight-range ([Natural Natural (U String Color%-Instance)]
                             [Boolean (U 'rectangle 'ellipse 'hollow-ellipse)]
                             . ->* . Void)]
         [unhighlight-ranges/key (Any -> Void)]
         [unhighlight-ranges ((Natural
                               Natural
                               Color%-Instance
                               Boolean
                               (U 'rectangle 'ellipse 'hollow-ellipse)
                               (U Boolean Natural)
                               Any
                               -> Boolean)
                              -> Void)]
         [get-highlighted-ranges (-> (Listof Any #|FIXME Text:Range?|#))]
         [get-styles-fixed (-> Boolean)]
         [get-fixed-style (-> (Instance Style<%>))]
         [set-styles-fixed (Boolean -> Void)]
         [move/copy-to-edit ([Text%-Instance Integer Integer Integer]
                             [#:try-to-move? Boolean]
                             . ->* . Void)]
         [initial-autowrap-bitmap
          (-> (Option (Instance Bitmap%)))]
         [get-port-name
          (-> (U Path-String Symbol #f))]
         [port-name-matches? (Any -> Boolean)]
         [get-edition-number (-> Natural)]
         [get-start-of-line (Natural -> Natural)]))

(define-type Text:Basic-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Text%
           #:implements Editor:Basic<%>)
    ->
    (Class #:row-var r #:implements Text:Basic<%>)))

(define-type Text:Line-Spacing<%>
  (Class #:implements Text:Basic<%>))

(define-type Text:Line-Spacing-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text:Basic<%>)
    ->
    (Class #:row-var r #:implements Text:Line-Spacing<%>)))

(define-type Text:First-Line<%>
  (Class #:implements Text%
         [highlight-first-line (Boolean -> Void)]
         [first-line-currently-drawn-specially? (-> Boolean)]
         [get-first-line-height (-> Natural)]
         [is-special-first-line? (String -> Boolean)]))

(define-type Text:First-Line-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text%)
    ->
    (Class #:row-var r #:implements Text:First-Line<%>)))

(define-type Text:Foreground-Color<%>
  (Class #:implements Text:Basic<%>
         #:implements Editor:Standard-Style-List<%>))

(define-type Text:Foreground-Color-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Text:Basic<%>
           #:implements Editor:Standard-Style-List<%>)
    ->
    (Class #:row-var r #:implements Text:Foreground-Color<%>)))

(define-type Text:Hide-Caret/Selection<%>
  (Class #:implements Text:Basic<%>))

(define-type Text:Hide-Caret/Selection-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text:Basic<%>)
    ->
    (Class #:row-var r #:implements Text:Hide-Caret/Selection<%>)))

(define-type Text:Nbsp->Space<%>
  (Class #:implements Text%))

(define-type Text:Nbsp->Space-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text%)
    ->
    (Class #:row-var r #:implements Text:Nbsp->Space<%>)))

(define-type Text:Column-Guide<%>
  (Class #:implements Text%))

(define-type Text:Column-Guide-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text%)
    ->
    (Class #:row-var r #:implements Text:Column-Guide<%>)))

(define-type Text:Normalize-Paste<%>
  (Class #:implements Text:Basic<%>
         [ask-normalize? (-> Boolean)]
         [string-normalize (String -> String)]))

(define-type Text:Normalize-Paste-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text:Basic<%>)
    ->
    (Class #:row-var r #:implements Text:Normalize-Paste<%>)))

(define-type Text:Searching<%>
  (Class #:implements Text:Basic<%>
         #:implements Editor:Keymap<%>
         [set-searching-state
          ((Option String) Boolean Boolean Boolean -> Void)]
         [set-search-anchor ((Option Natural) -> Void)]
         [get-search-hit-count (-> Natural)]
         [get-replace-search-hit (-> (Option Natural))]
         [set-replace-start ((Option Natural) -> Void)]
         [get-search-bubbles (-> (Listof (List (Pairof Natural Natural)
                                               (U 'normal-search-color
                                                  'dark-search-color
                                                  'light-search-color))))]))

(define-type Text:Searching-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Text:Basic<%>
           #:implements Editor:Keymap<%>)
    ->
    (Class #:row-var r #:implements Text:Searching<%>)))

(define-type Text:Return<%>
  (Class #:implements Text%))

(define-type Text:Return-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text%)
    ->
    (Class (init [return (-> Boolean)])
           #:row-var r #:implements Text:Return<%>)))

(define-type Text:Wide-Snip<%>
  (Class #:implements Text:Basic<%>
         [add-wide-snip (Snip%-Instance -> Void)]
         [add-tall-snip (Snip%-Instance -> Void)]))

(define-type Text:Wide-Snip-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text:Basic<%>)
    ->
    (Class #:row-var r #:implements Text:Wide-Snip<%>)))

(define-type Text:Delegate<%>
  (Class #:implements Text:Basic<%>
         [get-delegate (-> (Option Text%-Instance))]
         [set-delegate ((Option Text%-Instance) -> Void)]))

(define-type Text:1-Pixel-String-Snip%
  (Class #:implements String-Snip%)) ; FIXME: ok?

(define-type Text:1-Pixel-Tab-Snip%
  (Class #:implements Tab-Snip%)) ; FIXME: ok?

(define-type Text:Delegate-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text:Basic<%>)
    ->
    (Class #:row-var r #:implements Text:Delegate<%>)))

(define-type Text:Info<%>
  (Class #:implements Text:Basic<%>))

(define-type Text:Info-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Text:Basic<%>
           #:implements Editor:Keymap<%>)
    ->
    (Class #:row-var r #:implements Text:Info<%>)))

(define-type Text:Clever-File-Format<%>
  (Class #:implements Text%))

(define-type Text:Clever-File-Format-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text%)
    ->
    (Class #:row-var r #:implements Text:Clever-File-Format<%>)))

(define-type Text:Crlf-Line-Endings<%>
  (Class #:implements Text%))

(define-type Text:Crlf-Line-Endings-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text%)
    ->
    (Class #:row-var r #:implements Text:Crlf-Line-Endings<%>)))

(define-type Text:File<%>
  (Class #:implements Text:Basic<%>
         #:implements Editor:File<%>
         [get-read-write? (-> Boolean)]
         [while-unlocked ((-> Any) -> Any)]))

(define-type Text:File-Mixin
  (All (r #:row)
    (Class #:row-var r
           #:implements Text:Basic<%>
           #:implements Editor:File<%>)
    ->
    (Class #:row-var r #:implements Text:File<%>)))

(define-type Text:Ports<%>
  (Class [delete/io (Integer Integer -> Void)]
         [do-submission (-> Void)]
         [get-insertion-point (-> Integer)]
         [set-insertion-point (Integer -> Void)]
         [get-unread-start-point (-> Integer)]
         [set-unread-start-point (Integer -> Void)]
         [set-allow-edits (Boolean -> Void)]
         [get-allow-edits (-> Boolean)]
         [insert-between ((U Snip%-Instance String) -> Void)]
         [insert-before ((U Snip%-Instance String) -> Void)]
         [submit-to-port? (Key-Event%-Instance -> Boolean)]
         [on-sumit (-> Void)]
         [send-eof-to-in-port (-> Void)]
         [send-eof-to-box-in-port (-> Void)]
         [reset-input-box (-> Void)]
         [clear-output-ports (-> Void)]
         [clear-input-port (-> Void)]
         [clear-box-input-port (-> Void)]
         [get-out-style-delta (-> (U Style-Delta%-Instance String))]
         [get-err-style-delta (-> (U Style-Delta%-Instance String))]
         [get-value-style-delta (-> (U Style-Delta%-Instance String))]
         [get-in-port (-> Input-Port)]
         [get-in-box-port (-> Input-Port)]
         [get-out-port (-> Output-Port)]
         [get-err-port (-> Output-Port)]
         [get-value-port (-> Output-Port)]
         [after-io-insertion (-> Void)]
         [get-box-input-editor-snip% (-> Snip% #|Fixme (Subclass?/C editor-snip%)|#)]
         [get-box-input-text% (-> (Instance Text:Input-Box<%>))]))

(define-type Text:Ports-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text:Wide-Snip<%>)
    ->
    (Class #:row-var r #:implements Text:Ports<%>)))

(define-type Text:Input-Box<%>
  (Class #:implements Text%))

(define-type Text:Input-Box-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text%)
    ->
    (Class #:row-var r #:implements Text:Input-Box<%>)))

(define-type Text:Autocomplete<%>
  (Class #:implements Text%
         [auto-complete (-> Void)]
         [get-autocomplete-border-color (-> (U String Color%-Instance))]
         [get-autocomplete-background-color (-> (U String Color%-Instance))]
         [get-autocomplete-selected-color (-> (U String Color%-Instance))]
         [completion-mode-key-event? (Key-Event%-Instance -> Boolean)]
         [get-all-words (-> (Listof String))]
         [get-word-at (Positive-Integer -> String)]))

(define-type Text:Autocomplete-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text%)
    ->
    (Class #:row-var r #:implements Text:Autocomplete<%>)))

;; FIXME: Not sure about these classes' types
(define-type Text:Basic%
  (Class #:implements Text:Basic<%>))
(define-type Text:Line-Spacing%
  (Class #:implements Text:Line-Spacing<%>))
(define-type Text:Hide-Caret/Selection%
  (Class #:implements Text:Hide-Caret/Selection<%>))
(define-type Text:Nbsp->Space%
  (Class #:implements Text:Nbsp->Space<%>))
(define-type Text:Normalize-Paste%
  (Class #:implements Text:Normalize-Paste<%>))
(define-type Text:Delegate%
  (Class #:implements Text:Delegate<%>))
(define-type Text:Wide-Snip%
  (Class #:implements Text:Wide-Snip<%>))
(define-type Text:Standard-Style-List%
  (Class #:implements Editor:Standard-Style-List<%>))
(define-type Text:Input-Box%
  (Class #:implements Text:Input-Box<%>))
(define-type Text:Keymap%
  (Class #:implements Editor:Keymap<%>))
(define-type Text:Return%
  (Class #:implements Text:Return<%>))
(define-type Text:Autowrap%
  (Class #:implements Editor:Autowrap<%>))
(define-type Text:File%
  (Class #:implements Text:File<%>))
(define-type Text:Clever-File-Format%
  (Class #:implements Text:Clever-File-Format<%>))
(define-type Text:Backup-Autosave%
  (Class #:implements Editor:Backup-Autosave<%>))
(define-type Text:Searching%
  (Class #:implements Text:Searching<%>))
(define-type Text:Info%
  (Class #:implements Text:Info<%>))

(define-type Text:Line-Numbers<%>
  (Class [show-line-numbers! (Boolean -> Void)]
         [show-line-numbers? (-> Boolean)]
         [set-line-numbers-color (String -> Void)]))

(define-type Text:Line-Numbers-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Text%)
    ->
    (Class #:row-var r
           #:implements Text%
           #:implements Text:Line-Numbers<%>)))
