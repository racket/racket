#lang racket/gui

(require mrlib/aligned-pasteboard)

(define frame (new frame% [label "big-min"] [width 400] [height 500]))
(define test-suite (new vertical-pasteboard%))
(new aligned-editor-canvas% [parent frame] [editor test-suite])

(define top-string #f)

(define (new*)
  (define main-pb (new horizontal-pasteboard%))
  (define pb      (new vertical-pasteboard%))
  (define snip    (new aligned-editor-snip% [editor pb]))
  (define string   (make-object string-snip% "Testing String Snip"))
  (set! top-string string)
  (send main-pb insert snip false)
  (send main-pb insert string false)
  (new aligned-editor-snip% [editor main-pb]))

(send frame show #t)
(define (add) (send test-suite insert (new*)))
(add)
