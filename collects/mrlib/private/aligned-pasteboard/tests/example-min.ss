(require
 (lib "class.ss")
 (lib "mred.ss" "mred")
 (lib "etc.ss")
 "../aligned-pasteboard.ss"
 "../aligned-editor-container.ss")

(define f (new frame% (label "") (width 400) (height 400)))
(define e (new horizontal-pasteboard%))
(define c (new aligned-editor-canvas% (parent f) (editor e)))
(define vp1 (new vertical-pasteboard%))
(define ae-snip1 (new aligned-editor-snip% (editor vp1)))
(define t-snip1 (new editor-snip% (editor (new text%))))
(send e insert ae-snip1 false)
(send vp1 insert t-snip1 false)
(send f show true)