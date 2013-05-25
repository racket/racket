#lang racket/gui

(require "../aligned-pasteboard.rkt" "../aligned-editor-container.rkt")

(define frame (new frame% [label "Frame"] [width 400] [height 400]))
(define pasteboard (new horizontal-pasteboard%))
(define canvas (new aligned-editor-canvas% [parent frame] [editor pasteboard]))

(define vp1      (new vertical-pasteboard%))
(define ae-snip1 (new aligned-editor-snip% [editor vp1]))
(define vp2      (new vertical-pasteboard%))
(define ae-snip2 (new aligned-editor-snip% [editor vp2]))
(define vp3      (new vertical-pasteboard%))
(define ae-snip3 (new aligned-editor-snip% [editor vp3]))
(define vp4      (new vertical-pasteboard%))
(define ae-snip4 (new aligned-editor-snip% [editor vp4]))
(define vp5      (new vertical-pasteboard%))
(define ae-snip5 (new aligned-editor-snip% [editor vp5]))
(define t-snip1  (new editor-snip% [editor (instantiate text% ())]))
(define t-snip2  (new editor-snip% [editor (instantiate text% ())]))

(send pasteboard insert ae-snip1 false)
(send pasteboard insert ae-snip2 false)
(send pasteboard insert ae-snip5 false)
(send vp2 insert ae-snip3 false)
(send vp2 insert ae-snip4 false)
(send vp1 insert t-snip1 false)
(send vp5 insert t-snip2 false)
(send frame show true)
