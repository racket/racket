(require
 mzlib/class
 mzlib/etc
 mred
 "../verthoriz-alignment.rkt"
 "../lines.rkt"
 "../aligned-pasteboard.rkt"
 "../snip-wrapper.rkt")

(require "../snip-lib.rkt")

(define f (new frame% (label "f") (height 500) (width 500)))
(send f show true)
(define p (new aligned-pasteboard%))
(define c (new editor-canvas% (editor p) (parent f)))
(define a1 (new vertical-alignment% (parent p)))
(define a2 (new horizontal-alignment% (parent a1)))
(define a3 (new horizontal-alignment% (parent a1)))

(new snip-wrapper%
     (snip (make-object string-snip% "One"))
     (parent a2))
(new snip-wrapper%
     (snip (make-object string-snip% "Two"))
     (parent a2))
(new snip-wrapper%
     (snip (make-object string-snip% "Three"))
     (parent a3))
(new snip-wrapper%
     (snip (make-object string-snip% "Three"))
     (parent a3))
