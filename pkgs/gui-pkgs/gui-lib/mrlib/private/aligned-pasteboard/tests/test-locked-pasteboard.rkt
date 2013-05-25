#lang racket/gui

(require "../locked-pasteboard.rkt" mrlib/click-forwarding-editor)

(define f (new frame% [width 400] [height 500] [label "test"]))
(define e (new (click-forwarding-editor-mixin
                (locked-pasteboard-mixin pasteboard%))))
(define c (new editor-canvas% [parent f] [editor e]))
(define t (new text%))
(define s (new editor-snip% [editor t]))
(send e insert s 0 100)
(define t2 (new text%))
(define s2 (new editor-snip% [editor t2]))
(send e insert s2 100 0)
(send f show #t)
;; This test is not automated. To test it try to use the pasteboard that
;; appears.
;; (test:mouse-click 'left 0 100)
;; (test:keystroke #\A)
;; (string=? (send s get-text) "A")
;; (send f show #f)
