#lang typed/scheme/base

(require typed/private/utils)

(dt Bitmap% (Class (Real Real Boolean)
                   ()
                   ([get-width (-> Integer)]
                    [get-height (-> Integer)])))
(dt Font-List% (Class () () ([find-or-create-font
			      (case-lambda
			       (Integer Symbol Symbol Symbol -> (Instance Font%))
			       (Integer String Symbol Symbol Symbol -> (Instance Font%)))])))
(dt Font% (Class () () ([get-face (-> (Option String))]
                        [get-point-size (-> Integer)])))
(dt Dialog% (Class ()
                   ([parent Any] [width Integer] [label String])
                   ([show (Any -> Void)])))
(dt Text-Field% (Class ()
                       ([parent Any] [callback Any] [label String])
                       ([get-value (-> String)]
                        [focus (-> Void)])))
(dt Horizontal-Panel% (Class ()
                             ([parent Any]
                              [stretchable-height Any #t]
                              [alignment (List Symbol Symbol) #t])
                             ()))
(dt Choice% (Class ()
                   ([parent Any] [label String] [choices (Listof Any)] [callback Any])
                   ([get-selection (-> (Option Natural))]
                    [set-selection (Integer -> Any)]
                    [get-string-selection (-> (Option String))]
                    [set-string-selection (String -> Void)])))
(dt Message% (Class ()
                    ([parent Any] [label String])
                    ([set-label ((U String (Instance Bitmap%)) -> Void)])))
(dt Horizontal-Pane% (Class ()
                            ([parent Any])
                            ()))
(dt Editor-Canvas% (Class ()
                          ([parent Any] [editor Any])
                          ([set-line-count ((U #f Integer) -> Void)])))
(dt Bitmap-DC% (Class ((Instance Bitmap%))
                      ()
                      ([get-text-extent (String (Instance Font%) -> (values Real Real Real Real))]
                       [get-pixel (Number Number (Instance Color%) -> Boolean)]
                       [set-bitmap ((Option (Instance Bitmap%)) -> Void)]
                       [clear (-> Void)]
                       [set-font ((Instance Font%) -> Void)]
                       [draw-text (String Number Number -> Void)])))
(dt Color% (Class () () ([red (-> Number)])))

(dt Snip% (Class () () ([get-count (-> Integer)])))

(dt Text% (Class ()
		 ()
		 ([begin-edit-sequence (-> Void)]
		  [end-edit-sequence (-> Void)]
		  [lock (Boolean -> Void)]
		  [last-position (-> Number)]
		  [last-paragraph (-> Exact-Nonnegative-Integer)]
		  [delete (Number Number -> Void)]
		  [auto-wrap (Any -> Void)]
		  [paragraph-end-position (Number -> Integer)]
		  [paragraph-start-position (Number -> Integer)]
		  [get-start-position (-> Integer)]
		  [get-end-position (-> Integer)]
		  [while-unlocked ((-> Any) -> Any)]
		  [get-text (Integer (U Integer 'eof) -> String)]
		  [insert (String Number Number -> Void)])))

(dt Button% (Class () () ()))
(dt Event% (Class () () ()))

(require/typed/provide
 scheme/gui
 [button% Button%]
 [event% Event%]
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
 [color% Color%]
 [snip% Snip%]
 [open-input-text-editor
  ((Instance Text%) Integer (U 'end Integer) ((Instance Snip%) -> (Instance Snip%)) (Instance Text%) Boolean -> Input-Port)])

