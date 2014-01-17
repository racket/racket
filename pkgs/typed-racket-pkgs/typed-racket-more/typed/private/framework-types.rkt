#lang typed/racket

;; Types for the framework library

(require "../racket/private/gui-types.rkt")

;; 4 Canvas
(provide Canvas:Basic<%>
         Canvas:Basic%
         Canvas:Wide-Snip<%>
         Canvas:Wide-Snip-Mixin)

(define-type Canvas:Basic<%>
  (Class #:implements Editor-Canvas%))

(define-type Canvas:Basic%
  (Class #:implements Canvas:Basic<%>
         (init [parent (Instance Dialog%)]
               [editor (Instance Text:Basic<%>)])))

(define-type Canvas:Wide-Snip<%>
  (Class #:implements Canvas:Basic<%>
         [recalc-snips (-> Void)]
         [add-wide-snip ((Instance Snip%) -> Void)]
         [add-tall-snip ((Instance Snip%) -> Void)]))

(define-type Canvas:Wide-Snip-Mixin
  (All (r #:row)
    (Class #:row-var r #:implements Canvas:Basic<%>)
    ->
    (Class #:row-var r #:implements Canvas:Wide-Snip<%>)))

;; 11 Editor
(provide Editor:Basic<%>
         Editor:Keymap<%>
         Editor:File<%>)

(define-type Editor:Basic<%>
  (Class #:implements Editor<%>
         [has-focus? (-> Boolean)]
         ;; FIXME
         ))

(define-type Editor:Keymap<%>
  (Class #:implements Editor:Basic<%>
         ;; FIXME
         ))

(define-type Editor:File<%>
  (Class #:implements Editor:Keymap<%>
         ;; FIXME
         [update-frame-filename (-> Void)]
         [allow-close-with-no-filename? (-> Boolean)]
         [user-saves-or-not-modified? (#t -> Boolean)] ; FIXME: fishy docs
         ))

;; 14 Frame
(provide Frame:Basic<%>
         Frame:Focus-Table<%>
         Frame:Size-Pref<%>
         Frame:Register-Group<%>
         Frame:Status-Line<%>
         Frame:Basic-Mixin
         Frame:Focus-Table-Mixin
         Frame:Size-Pref-Mixin
         Frame:Register-Group-Mixin
         Frame:Status-Line-Mixin)

(define-type Frame:Basic<%>
  (Class #:implements Frame%
         ;; this method has a tricky type
         [get-area-container% (-> Any)]
         [get-area-container (-> (Instance Area-Container<%>))]
         [get-menu-bar% (-> Any)]
         [make-root-area-container
          (Any (Instance Area-Container<%>) -> (Instance Area-Container<%>))]
         [close (-> Void)]
         [editing-this-file? (Path -> Boolean)]
         [get-filename
          (case->
           (-> (Option Path))
           ((Option (Boxof Boolean)) -> (Option Path)))]
         [make-visible (String -> Void)]))

(define-type Frame:Focus-Table<%>
  (Class #:implements Frame%))

(define-type Frame:Size-Pref<%>
  (Class #:implements Frame:Basic<%>
         [adjust-size-when-monitor-setup-changes? (-> Boolean)]))

(define-type Frame:Register-Group<%>
  (Class #:implements Frame%))

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

(define-type Frame:Basic-Mixin
  (All (r #:row)
       (Class #:row-var r #:implements Frame%)
       ->
       (Class #:row-var r #:implements Frame:Basic<%>)))

(define-type Frame:Focus-Table-Mixin
  (All (r #:row)
       (Class #:row-var r #:implements Frame%)
       ->
       (Class #:row-var r #:implements Frame:Focus-Table<%>)))

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

(define-type Frame:Register-Group-Mixin
  (All (r #:row)
       (Class #:row-var r #:implements Frame:Basic<%>)
       ->
       (Class #:row-var r #:implements Frame:Focus-Table<%>)))

(define-type Frame:Status-Line-Mixin
  (All (r #:row)
       (Class #:row-var r #:implements Frame:Basic<%>)
       ->
       (Class #:row-var r #:implements Frame:Status-Line<%>)))

;; 29 Text
(provide Text:Basic<%>
         Text:File<%>)

(define-type Text:Basic<%>
  (Class #:implements Text%
         ;; highlight-range
         ;; unhighlight-range
         ;; unhighlight-ranges/key
         [unhighlight-ranges/key (Any -> Void)]
         ;; unhighlight-ranges
         ;; get-highlighted-ranges
         [get-styles-fixed (-> Boolean)]
         ;; get-fixed-style
         [set-styles-fixed (Boolean -> Void)]
         ;; move/copy-to-edit
         [initial-autowrap-bitmap
          (-> (Option (Instance Bitmap%)))]
         [get-port-name
          (-> (U Path-String Symbol #f))]
         [port-name-matches? (Any -> Boolean)]
         [get-edition-number (-> Natural)]
         [get-start-of-line (Natural -> Natural)]))

(define-type Text:File<%>
  (Class #:implements Text:Basic<%>
         #:implements Editor:File<%>
         [get-read-write? (-> Boolean)]
         [while-unlocked ((-> Any) -> Any)]))

