#lang typed/racket/base

(require racket/class
         typed/private/utils)

(provide Frame%
         Bitmap%
         Font-List%
         Font%
         Dialog%
         Text-Field%
         Horizontal-Panel%
         Choice%
         Message%
         Horizontal-Pane%
         Editor-Canvas%
         Bitmap-DC%
         Color%
         Snip%
         Text:Basic%
         Text%
         Button%
         Event%)

(define-type Frame%
  (Rec This
    (Class (init [label String]
                 [parent (Option This) #:optional]
                 [width (Option Integer) #:optional]
                 [height (Option Integer) #:optional]
                 [x (Option Integer) #:optional]
                 [y (Option Integer) #:optional]
                 ;; more
                 )
           [show (Any -> Void)])))

(define-type Bitmap%
  (Class (init-rest (U (List Exact-Positive-Integer Exact-Positive-Integer)
                       (List Exact-Positive-Integer Exact-Positive-Integer Any)
                       (List Exact-Positive-Integer Exact-Positive-Integer Any Any)
                       (List Exact-Positive-Integer Exact-Positive-Integer Any Any Real)))
         [get-width (-> Integer)]
         [get-height (-> Integer)]))

(define-type Font-List%
  (Class
   [find-or-create-font
    (case-> (Integer Symbol Symbol Symbol -> (Instance Font%))
            (Integer String Symbol Symbol Symbol -> (Instance Font%))
            (Integer String Symbol Symbol Symbol Symbol -> (Instance Font%)))]))

(define-type Font%
  (Class [get-face (-> (Option String))]
         [get-point-size (-> Integer)]))

(define-type Dialog%
  (Class (init [label String]
               [parent Any #:optional]
               [width Integer #:optional])
         [show (Any -> Void)]))

(define-type Text-Field%
  (Class (init [label String]
               [parent (Instance Dialog%)]
               [callback (Any Any -> Any) #:optional])
         [get-value (-> String)]
         [focus (-> Void)]))

(define-type Horizontal-Panel%
  (Class (init [parent (Instance Dialog%)]
               [stretchable-height Any #:optional]
               [alignment (List Symbol Symbol) #:optional])))

(define-type Choice%
  (Class (init [parent (Instance Horizontal-Panel%)]
               [label String]
               [choices (Listof Any)]
               [callback (Any Any -> Any)])
         [get-selection (-> (Option Natural))]
         [set-selection (Integer -> Any)]
         [get-string-selection (-> (Option String))]
         [set-string-selection (String -> Void)]))

(define-type Message%
  (Class (init [parent (Instance Horizontal-Panel%)]
               [label String])
         [set-label ((U String (Instance Bitmap%)) -> Void)]))

(define-type Horizontal-Pane%
  (Class (init [parent (Instance Horizontal-Panel%)])))

(define-type Editor-Canvas%
  (Class (init [parent (Instance Dialog%)]
               [editor (Instance Text:Basic%)])
         [set-line-count ((U #f Integer) -> Void)]))

(define-type Bitmap-DC%
  (Class (init [bitmap (Instance Bitmap%)])
         [get-text-extent (String (Instance Font%) ->
                                    (values Nonnegative-Real Nonnegative-Real
                                            Nonnegative-Real Nonnegative-Real))]
         [get-pixel (Number Number (Instance Color%) -> Boolean)]
         [set-bitmap ((Option (Instance Bitmap%)) -> Void)]
         [clear (-> Void)]
         [set-font ((Instance Font%) -> Void)]
         [draw-text (String Number Number -> Void)]))

(define-type Color%
  (Class (init-rest (U Null
                       (List Byte Byte Byte)
                       (List Byte Byte Byte Real)))
         [red (-> Number)]))

(define-type Snip%
  (Class [get-count (-> Integer)]))

(define-type Text:Basic%
    (Class [begin-edit-sequence (-> Void)]
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
           [get-text (Integer (U Integer 'eof) -> String)]
           [insert (String Number Number -> Void)]))

(define-type Text%
  (Class [begin-edit-sequence (-> Void)]
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
         [insert (String Number Number -> Void)]))

(define-type Button%
  (Rec B%
    (Class (init [label String]
                 [parent (Instance Frame%)]
                 [callback (B% Any -> Any)]))))

(define-type Event%
  (Class))

(require/typed/provide
 racket/gui
 [frame% Frame%]
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
 [make-bitmap
  (case->
   (Exact-Positive-Integer Exact-Positive-Integer -> (Instance Bitmap%))
   (Exact-Positive-Integer Exact-Positive-Integer Any -> (Instance Bitmap%)))]
 [color% Color%]
 [make-color
  (case->
   (Byte Byte Byte -> (Instance Color%))
   (Byte Byte Byte Real -> (Instance Color%)))]
 [snip% Snip%]
 [message-box (String String -> (U 'ok 'cancel 'yes 'no))]
 [open-input-text-editor
  ((Instance Text%) Integer (U 'end Integer) ((Instance Snip%) -> (Instance Snip%)) (Instance Text%) Boolean -> Input-Port)])

