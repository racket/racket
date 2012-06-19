#lang racket/gui

(require "../aligned-editor-container.rkt" "../aligned-pasteboard.rkt")

(define pb (new horizontal-pasteboard%))
(send* pb
  (insert (make-object string-snip% "Call") #f)
  (insert (new editor-snip% [editor (new text%)]) #f))
(define z (new aligned-editor-snip% [editor pb]))
(define f (new frame% [label "more-tests-text"] [width 200] [height 200]))
(define e (new pasteboard%))
(define c (new editor-canvas% [editor e] [parent f]))
(send e insert z)
(send f show #t)

;;;;;;;;;;
;; exploration
(require "../snip-lib.rkt")
(define (margin snip)
  (define left   (box 0))
  (define top    (box 0))
  (define right  (box 0))
  (define bottom (box 0))
  (send snip get-margin left top right bottom)
  (list (cons 'left   (unbox left))
        (cons 'right  (unbox right))
        (cons 'top    (unbox top))
        (cons 'bottom (unbox bottom))))
