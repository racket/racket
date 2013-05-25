#lang racket/gui

(require "../aligned-pasteboard.rkt" "../aligned-editor-container.rkt")

(define f (new frame% (label "") (width 400) (height 400)))
(define e (new horizontal-pasteboard%))
(define c (new aligned-editor-canvas% (parent f) (editor e)))
(define vp1 (new vertical-pasteboard%))
(define ae-snip1 (new aligned-editor-snip% (editor vp1)))
(define t-snip1 (new editor-snip% (editor (new text%))))
(send e insert ae-snip1 false)
(send vp1 insert t-snip1 false)
(send f show true)
