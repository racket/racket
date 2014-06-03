#lang racket/gui

(require "test-macro.rkt" "../snip-lib.rkt"
         "../aligned-pasteboard.rkt" "../aligned-editor-container.rkt")

;; (printf "running tests for snip-lib.rkt\n")

;; snip-width: ((is-a?/c aligned-pasteboard<%>) (is-a?/c snip%) . -> . number?)
;; the width of a snip in the given pasteboard
(let* ([pb1 (new vertical-pasteboard%)]
       [es1 (new editor-snip% [editor pb1])]
       [pb2 (new vertical-pasteboard%)]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb2])])
  (send frame show #t)
  (send pb2 insert es1)
  (send es1 resize 20 20)
  (sleep/yield 0.3)
  (test equal? (snip-width #;pb2 es1) 20.0)
  (send es1 resize 200 90)
  (sleep/yield 0.3)
  (test equal? (snip-width #;pb2 es1) 200.0)
  (send frame show #f))

;; snip-height:
;; ((is-a?/c aligned-pasteboard<%>) (is-a?/c snip%) . -> . number?)
;; the height of a snip in the given pasteboard
(let* ([pb1 (new vertical-pasteboard%)]
       [es1 (new editor-snip% [editor pb1])]
       [pb2 (new vertical-pasteboard%)]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb2])])
  (send frame show #t)
  (send pb2 insert es1)
  (send es1 resize 20 20)
  (sleep/yield 0.3)
  (test equal? (snip-height #;pb2 es1) 20.0)
  (send es1 resize 200 90)
  (sleep/yield 0.3)
  (test equal? (snip-height #;pb2 es1) 90.0)
  (send frame show #f))

;; snip-min-width: ((is-a?/c snip%) . -> . number?)
;; the minimum width of the snip

;; snip-min-height: ((is-a?/c snip%) . -> . number?)
;; the minimum height of the snip

;; snip-parent: ((is-a?/c snip%) . -> . (is-a?/c editor<%>))
;; the pasteboard that contains the snip
(let* ([pb1 (new pasteboard%)]
       [es1 (new editor-snip% [editor pb1])]
       [pb2 (new pasteboard%)]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new editor-canvas% [parent frame] [editor pb2])])
  (send frame show #t)
  (send pb2 insert es1)
  (test equal? (snip-parent es1) pb2)
  (send frame show #f))

(let* ([pb1 (new horizontal-pasteboard%)]
       [pb2 (new horizontal-pasteboard%)]
       [pb3 (new horizontal-pasteboard%)]
       [pb4 (new horizontal-pasteboard%)]
       [pb5 (new horizontal-pasteboard%)]
       [es2 (new aligned-editor-snip% [editor pb2])]
       [es3 (new aligned-editor-snip% [editor pb3])]
       [es4 (new aligned-editor-snip% [editor pb4])]
       [es5 (new aligned-editor-snip% [editor pb5])]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb1])])
  (send frame show #t)
  (send pb1 insert es2)
  (send pb2 insert es3)
  (send pb3 insert es4)
  (send pb4 insert es5)
  (test equal? (snip-parent es2) pb1)
  (test equal? (snip-parent es3) pb2)
  (test equal? (snip-parent es4) pb3)
  (test equal? (snip-parent es5) pb4)
  (send frame show #f))

;; fold-snip: (lambda (b?) ((any? b? . -> . b?) b? (is-a?/c snip%) . -> . b?))
;; the application of f on all snips from snip to the end in a foldl
;; foldr mannor
(let* ([pb1 (new vertical-pasteboard%)]
       [es1 (new editor-snip% [editor (new text%)])]
       [es2 (new editor-snip% [editor (new text%)])]
       [es3 (new editor-snip% [editor (new text%)])]
       [es4 (new editor-snip% [editor (new text%)])]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb1])])
  (send frame show #t)
  (send pb1 insert es1)
  (send pb1 insert es2)
  (send pb1 insert es3)
  (send pb1 insert es4)
  (send es1 resize 100 100)
  (send es2 resize 100 100)
  (send es3 resize 100 100)
  (send es4 resize 100 100)
  (test = (fold-snip (λ (snip total-height)
                       (+ (snip-height #;pb1 snip) total-height))
                     0 es1)
        400)
  (send frame show #f))

;; for-each-snip:
;; (((is-a?/c snip%) . -> . (void)) (is-a/c? snip%) . -> . (void))
;; applies the function to all the snips
(let* ([pb1 (new vertical-pasteboard%)]
       [es1 (new editor-snip% [editor (new text%)])]
       [es2 (new editor-snip% [editor (new text%)])]
       [es3 (new editor-snip% [editor (new text%)])]
       [es4 (new editor-snip% [editor (new text%)])]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb1])]
       [count 0])
  (send frame show #t)
  (send pb1 insert es1)
  (send pb1 insert es2)
  (send pb1 insert es3)
  (send pb1 insert es4)
  (for-each-snip (λ (snip) (set! count (add1 count))) es1)
  (test = count 4)
  (send frame show #f))

;; (printf "tests done\n")
