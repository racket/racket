#lang typed-scheme
             
;(require mred/mred)
(provide (all-defined-out))

(define-type-alias Bitmap% (Class (Number Number Boolean)
                                  () 
                                  ([get-width (-> Number)]
                                   [get-height (-> Number)])))
(define-type-alias Font-List% (Class () () ([find-or-create-font (Any * -> (Instance Font%))])))
(define-type-alias Font% (Class () () ([get-face (-> (Option String))]
                                       [get-point-size (-> Number)])))
(define-type-alias Dialog% (Class () 
                                  ([parent Any] [width Number] [label String]) 
                                  ([show (Any -> Void)])))
(define-type-alias Text-Field% (Class () 
                                      ([parent Any] [callback Any] [label String])
                                      ([get-value (-> String)]
                                       [focus (-> String)])))
(define-type-alias Horizontal-Panel% (Class ()
                                            ([parent Any] 
                                             [stretchable-height Any #t]
                                             [alignment (List Symbol Symbol) #t])
                                            ()))
(define-type-alias Choice% (Class ()
                                  ([parent Any] [label String] [choices List] [callback Any])
                                  ([get-string-selection (-> (Option String))]
                                   [set-string-selection (String -> Void)])))
(define-type-alias Message% (Class ()
                                   ([parent Any] [label String])
                                   ([set-label ((U String (Instance Bitmap%)) -> Void)])))
(define-type-alias Horizontal-Pane% (Class ()
                                           ([parent Any])
                                           ()))
(define-type-alias Editor-Canvas% (Class ()
                                         ([parent Any] [editor Any])
                                         ([set-line-count (Number -> Void)])))
(define-type-alias Bitmap-DC% (Class ((Instance Bitmap%))
                                     ()
                                     ([get-text-extent (String (Instance Font%) -> (values Number Number Number Number))]
                                      [get-pixel (Number Number (Instance Color%) -> Boolean)]
                                      [set-bitmap ((Option (Instance Bitmap%)) -> Void)]
                                      [clear (-> Void)]
                                      [set-font ((Instance Font%) -> Void)]
                                      [draw-text (String Number Number -> Void)])))
(define-type-alias Color% (Class () () ([red (-> Number)])))
(define-type-alias Style-List% (Class () 
                                      ()
                                      ([find-named-style 
                                        (String -> (Instance (Class ()
                                                                    () 
                                                                    ([get-font (-> (Instance Font%))]))))])))

(define-type-alias Scheme:Text% (Class ()
                                       ()
                                       ([begin-edit-sequence (-> Void)]
                                        [end-edit-sequence (-> Void)]
                                        [lock (Boolean -> Void)]
                                        [last-position (-> Number)]
                                        [last-paragraph (-> Number)]
                                        [delete (Number Number -> Void)]
                                        [auto-wrap (Any -> Void)]
                                        [paragraph-end-position (Number -> Number)]
                                        [paragraph-start-position (Number -> Number)]
                                        [get-start-position (-> Number)]
                                        [get-end-position (-> Number)]
                                        [insert (String Number Number -> Void)])))

(require/typed mred/mred
               [the-font-list (Instance Font-List%)]
               [dialog% Dialog%]
               [text-field% Text-Field%]
               [horizontal-panel% Horizontal-Panel%]
               [choice% Choice%]
               [get-face-list (-> (Listof String))]
               [message% Message%]
               [horizontal-pane% Horizontal-Pane%]
               [editor-canvas% Editor-Canvas%]
               [bitmap-dc% Bitmap-DC%]
               [bitmap% Bitmap%]
               [color% Color%])

(require/typed framework/framework
               [preferences:set-default (Symbol Any Any -> Void)]
               [preferences:set (Symbol Any -> Void)]
               [editor:get-standard-style-list 
                (-> (Instance Style-List%))]
               [scheme:text% Scheme:Text%]
               [gui-utils:ok/cancel-buttons (Any (Any Any -> Any) (Any Any -> Any) -> (values Any Any))])

(require/typed "prefs-contract.ss"
               [preferences:get-drscheme:large-letters-font (-> (U #f (Pair String Number)))])

(require (only-in "prefs-contract.ss" preferences:get))
(provide preferences:get preferences:get-drscheme:large-letters-font)

(define-type-alias Bitmap-Message% (Class () 
                                          ([parent Any])
                                          ([set-bm ((Instance Bitmap%) -> Void)])))


(require/typed "bitmap-message.ss"
               [bitmap-message% Bitmap-Message%])
