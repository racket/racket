#lang racket/gui

(require "../aligned-editor-container.rkt" "../aligned-pasteboard.rkt")

(define f  (new frame% [label "test"] [width 200] [height 200]))
(define e  (new vertical-pasteboard%))
(define c  (new aligned-editor-canvas% [editor e] [parent f]))
(define pb (new vertical-pasteboard%))
(define s  (new aligned-editor-snip%
                [editor pb] [stretchable-height #f] [stretchable-width #f]))
(send pb insert (make-object string-snip% "Long snip"))
(send pb insert (make-object string-snip% "Longer snip"))
(send e insert s)
(send f show #t)
