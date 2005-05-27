(require (lib "aligned-pasteboard.ss" "embedded-gui")
         (lib "debug.ss" "mike-lib"))

(define f (new frame% (label "f") (width 100) (height 100)))
(define e (new text%))
(define c (new editor-canvas% (parent f) (editor e)))
(define main (new aligned-pasteboard%))
(define j (new editor-snip% (editor main)))
(define line (new horizontal-alignment% (parent main)))
(send e insert j)
(zero? (send line get-min-width))
(send line add (make-object string-snip% "foo"))
(send line add (make-object string-snip% "foo"))
(send f show #t)
;; If the following test case is delayed it's true.
;; is this a problem? It could be if this attempt
;; to read the min-width when aligning. However, this
;; program prints out foofoo like it should and doesn't
;; overlap them so maybe we're okay.
(not (zero? (send line get-min-width)))
