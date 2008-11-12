#lang typed-scheme

(define-syntax-rule (dt nm t)
  (begin (define-type-alias nm t) (provide nm)))

(define-syntax-rule (require/typed/provide lib [nm t] ...)
  (begin
    (require/typed lib [nm t] ...)
    (provide nm ...)))

(provide dt require/typed/provide)

(dt Bitmap% (Class (Number Number Boolean)
                   () 
                   ([get-width (-> Number)]
                    [get-height (-> Number)])))
(dt Font-List% (Class () () ([find-or-create-font (Any * -> (Instance Font%))])))
(dt Font% (Class () () ([get-face (-> (Option String))]
                        [get-point-size (-> Number)])))
(dt Dialog% (Class () 
                   ([parent Any] [width Number] [label String]) 
                   ([show (Any -> Void)])))
(dt Text-Field% (Class () 
                       ([parent Any] [callback Any] [label String])
                       ([get-value (-> String)]
                        [focus (-> String)])))
(dt Horizontal-Panel% (Class ()
                             ([parent Any] 
                              [stretchable-height Any #t]
                              [alignment (List Symbol Symbol) #t])
                             ()))
(dt Choice% (Class ()
                   ([parent Any] [label String] [choices List] [callback Any])
                   ([get-string-selection (-> (Option String))]
                    [set-string-selection (String -> Void)])))
(dt Message% (Class ()
                    ([parent Any] [label String])
                    ([set-label ((U String (Instance Bitmap%)) -> Void)])))
(dt Horizontal-Pane% (Class ()
                            ([parent Any])
                            ()))
(dt Editor-Canvas% (Class ()
                          ([parent Any] [editor Any])
                          ([set-line-count (Number -> Void)])))
(dt Bitmap-DC% (Class ((Instance Bitmap%))
                      ()
                      ([get-text-extent (String (Instance Font%) -> (values Number Number Number Number))]
                       [get-pixel (Number Number (Instance Color%) -> Boolean)]
                       [set-bitmap ((Option (Instance Bitmap%)) -> Void)]
                       [clear (-> Void)]
                       [set-font ((Instance Font%) -> Void)]
                       [draw-text (String Number Number -> Void)])))
(dt Color% (Class () () ([red (-> Number)])))


(require/typed/provide mred/mred
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

