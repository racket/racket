#lang racket/gui

(require mrlib/aligned-pasteboard)

(define f (new frame% [label "test"] [width 400] [height 500]))
(define pb1 (new vertical-pasteboard%))
(define ec (new aligned-editor-canvas%
                [parent f] [editor pb1] [style '(no-hscroll)]))

(define pb2 (new vertical-pasteboard%))
(define es2 (new aligned-editor-snip% [editor pb2]))

(define t (new text%))
(define es3 (new editor-snip% [editor t]))

(send pb1 insert es2)
(send pb2 insert es3)
(send f show true)
