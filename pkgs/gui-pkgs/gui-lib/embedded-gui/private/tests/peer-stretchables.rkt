(require
 mzlib/class
 mzlib/etc
 mred
 "../stretchable-editor-snip.rkt"
 "../verthoriz-alignment.rkt"
 "../aligned-pasteboard.rkt")

(define f (new frame% (label "f") (width 400) (height 400)))
(define e (new text%))
(define c (new editor-canvas% (parent f) (editor e)))
(define main (new aligned-pasteboard%))
(define horiz (new vertical-alignment% (parent main)))
(define es (new editor-snip% (editor main)))
(define ses1 (new stretchable-editor-snip% (editor (new text%))))
(define ses2 (new stretchable-editor-snip% (editor (new text%))))
(send* horiz (add ses1) (add ses2))
(send e insert es)
(send f show true)

(let ([e (send ses2 get-editor)]
      [ses1-mw (send ses1 get-min-width)])
  (send e insert "sdflsdfnsknbskdlf")
  (send e erase)
  (equal? ses1-mw (send ses1 get-min-width)))
