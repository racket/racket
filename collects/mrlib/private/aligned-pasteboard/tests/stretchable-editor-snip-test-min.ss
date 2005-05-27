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
(define t2 (new text%))
(define ses (new stretchable-editor-snip%
                 (editor t2)
                 (min-width 100)
                 (stretchable-width #t)
                 (stretchable-height #f)))
(send e insert aes)
(send pb insert ses)

(send f show #t)