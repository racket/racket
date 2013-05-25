#lang racket/gui

(require "../aligned-pasteboard.rkt" "../aligned-editor-container.rkt")

(define f        (new frame% (label "test")))
(define e        (new text%))
(define c        (new editor-canvas% (editor e) (parent f)))
(define pb       (new vertical-pasteboard%))
(define actual   (new text%))
(define act-line (new aligned-editor-snip% (editor (new vertical-pasteboard%))))
(define t        (new aligned-editor-snip% (editor pb)))
(send e insert t)
(send* pb (begin-edit-sequence) (insert act-line #f) (end-edit-sequence))

;; Eli: for some reason, this used to pass the result into this
;; function:
;;   (define (converges? x) #t)
;; I take it that the requirement is that it finishes after some time,
;; so there's no need for the function.
