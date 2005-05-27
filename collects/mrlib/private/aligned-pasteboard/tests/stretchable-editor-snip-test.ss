(require
 "../aligned-pasteboard.ss"
 "../aligned-editor-container.ss"
 "../stretchable-editor-snip.ss"
 "../snip-lib.ss")

(define f (new frame% (label "") (width 500) (height 500)))
(define e (new vertical-pasteboard%))
(define c (new aligned-editor-canvas% (parent f) (editor e)))

(define pb (new vertical-pasteboard%))
(define aes (new aligned-editor-snip%
                 (editor pb)
                 (stretchable-width #f)
                 (stretchable-height #f)))
(define t1 (new text%))
(define es (new editor-snip% (editor t1)))
(define t2 (new text%))
(define ses (new stretchable-editor-snip%
                 (editor t2)
                 (stretchable-width #t)
                 (stretchable-height #f)))

(send t1 insert "String")
(send e insert aes)
(send pb insert es)
(send pb insert ses)

(send f show #t)
(equal? (snip-width es) (snip-width ses))