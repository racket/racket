#lang racket/gui

(require "test-macro.rkt" "../pasteboard-lib.rkt"
         "../aligned-pasteboard.rkt" "../aligned-editor-container.rkt")

;; (printf "running tests for pasteboard-lib.rkt\n")

;; pasteboard-root:
;; ((is-a?/c aligned-pasteboard<%>) -> (is-a?/c aligned-pasteboard<%>))
;; gets the top most aligned pasteboard in the tree of pasteboards and
;; containers

(let* ([pb1 (new vertical-pasteboard%)]
       [pb2 (new horizontal-pasteboard%)]
       [pb3 (new vertical-pasteboard%)]
       [es2 (new aligned-editor-snip% [editor pb2])]
       [es3 (new aligned-editor-snip% [editor pb3])]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb1])])
  (send pb1 insert es2)
  (send pb2 insert es3)
  ;;
  (test equal? (pasteboard-root pb3) pb1)
  (test equal? (pasteboard-root pb2) pb1)
  (test equal? (pasteboard-root pb1) pb1))

(let* ([pb1 (new vertical-pasteboard%)]
       [pb2 (new horizontal-pasteboard%)]
       [pb3 (new vertical-pasteboard%)]
       [es1 (new aligned-editor-snip% [editor pb1])]
       [es3 (new aligned-editor-snip% [editor pb3])]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb2])])
  (send pb2 insert es1)
  (send pb2 insert es3)
  ;;
  (test equal? (pasteboard-root pb3) pb2)
  (test equal? (pasteboard-root pb2) pb2)
  (test equal? (pasteboard-root pb1) pb2))

;;pasteboard-parent: ((is-a?/c pasteboard%) . -> . (is-a?/c aligned-editor-container<%>))
;;gets the canvas or snip that the pasteboard is displayed in
(let* ([pb1 (new vertical-pasteboard%)]
       [pb2 (new horizontal-pasteboard%)]
       [pb3 (new vertical-pasteboard%)]
       [es2 (new aligned-editor-snip% [editor pb2])]
       [es3 (new aligned-editor-snip% [editor pb3])]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb1])])
  (send pb1 insert es2)
  (send pb2 insert es3)
  ;;
  (test equal? (pasteboard-parent pb1) canvas)
  (test equal? (pasteboard-parent pb2) es2)
  (test equal? (pasteboard-parent pb3) es3))

(let* ([pb1 (new vertical-pasteboard%)]
       [pb2 (new horizontal-pasteboard%)]
       [pb3 (new vertical-pasteboard%)]
       [es2 (new aligned-editor-snip% [editor pb2])]
       [es3 (new aligned-editor-snip% [editor pb3])]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb1])])
  (send pb1 insert es2)
  (send pb1 insert es3)
  ;;
  (test equal? (pasteboard-parent pb1) canvas)
  (test equal? (pasteboard-parent pb2) es2)
  (test equal? (pasteboard-parent pb3) es3))

#|
;; num-sizeable: ((is-a?/c aligned-pasteboard<%>) . -> . number?)
;; the number of snips in the pasteboard that can be resized
(let* ([pb1 (new vertical-pasteboard% ())]
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
  ;;
  (test = (num-sizeable pb1) 0)
  (send pb1 delete es1)
  (test = (num-sizeable pb1) 0)
  (send frame show #f))

(let* ([pb1 (new vertical-pasteboard%)]
       [es1 (new aligned-editor-snip% [editor (new vertical-pasteboard%)])]
       [es2 (new aligned-editor-snip% [editor (new vertical-pasteboard%)])]
       [es3 (new aligned-editor-snip% [editor (new vertical-pasteboard%)])]
       [es4 (new aligned-editor-snip% [editor (new vertical-pasteboard%)])]
       [frame (new frame% [label "l"] [width 10] [height 10])]
       [canvas (new aligned-editor-canvas% [parent frame] [editor pb1])])
  (send frame show #t)
  (send pb1 insert es1)
  (send pb1 insert es2)
  (send pb1 insert es3)
  (send pb1 insert es4)
  ;;
  (test = (num-sizeable pb1) 4)
  (send pb1 delete es1)
  (test = (num-sizeable pb1) 3)
  (send pb1 erase)
  (test = (num-sizeable pb1) 0)
  (send frame show #f))

(printf "tests done\n")
|#
