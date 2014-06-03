#lang racket/gui

(require "../aligned-editor-container.rkt" "../aligned-pasteboard.rkt"
         "../snip-lib.rkt")

(define f (new frame% [label "test"] [width 200] [height 200]))
(define e (new text%))
(define c (new editor-canvas% [editor e] [parent f]))

(define vpb1 (new vertical-pasteboard%))
(define aes1 (new aligned-editor-snip% [editor vpb1]))

(define vpb2 (new vertical-pasteboard%))
(define aes2 (new aligned-editor-snip% [editor vpb2]))

(define t  (new text%))
(define es (new editor-snip% (editor t)))

(send vpb1 insert aes2 false)
(send vpb2 insert es)
(send e insert aes1)
(send f show #t)
(sleep 0.2)
(send f show #f)

(send t begin-edit-sequence)
(send e begin-edit-sequence)
(send t insert "1\n")
(send t insert "1")
(send e end-edit-sequence)
(send t end-edit-sequence)

(>= (send vpb1 get-aligned-min-height)
    (send vpb2 get-aligned-min-height)
    (snip-height es))
