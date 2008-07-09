(require
 "utils.ss"
 mzlib/etc
 mzlib/class
 mred
 mrlib/private/aligned-pasteboard/pasteboard-lib
 mrlib/aligned-pasteboard)

;; pasteboard-root: ((is-a?/c aligned-pasteboard<%>) -> (is-a?/c aligned-pasteboard<%>))
;; gets the top most aligned pasteboard in the tree of pasteboards and containers

(let*
    ([pb1 (instantiate vertical-pasteboard% ())]
     [pb2 (instantiate horizontal-pasteboard% ())]
     [pb3 (instantiate vertical-pasteboard% ())]
     [es2 (instantiate aligned-editor-snip% () (editor pb2))]
     [es3 (instantiate aligned-editor-snip% () (editor pb3))]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate aligned-editor-canvas% () (parent frame) (editor pb1))])
  
  (send pb1 insert es2)
  (send pb2 insert es3)
  
  (test equal?
   (pasteboard-root pb3)
   pb1)
  
  (test equal?
   (pasteboard-root pb2)
   pb1)
  
  (test equal?
   (pasteboard-root pb1)
   pb1)
  )

(let*
    ([pb1 (instantiate vertical-pasteboard% ())]
     [pb2 (instantiate horizontal-pasteboard% ())]
     [pb3 (instantiate vertical-pasteboard% ())]
     [es1 (instantiate aligned-editor-snip% () (editor pb1))]
     [es3 (instantiate aligned-editor-snip% () (editor pb3))]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate aligned-editor-canvas% () (parent frame) (editor pb2))])
  
  (send pb2 insert es1)
  (send pb2 insert es3)
  
  (test equal?
   (pasteboard-root pb3)
   pb2)
  
  (test equal?
   (pasteboard-root pb2)
   pb2)
  
  (test equal?
   (pasteboard-root pb1)
   pb2)
  )

;; pasteboard-parent: ((is-a?/c pasteboard%) . -> . (is-a?/c aligned-editor-container<%>))
;; gets the canvas or snip that the pasteboard is displayed in
(let*
    ([pb1 (instantiate vertical-pasteboard% ())]
     [pb2 (instantiate horizontal-pasteboard% ())]
     [pb3 (instantiate vertical-pasteboard% ())]
     [es2 (instantiate aligned-editor-snip% () (editor pb2))]
     [es3 (instantiate aligned-editor-snip% () (editor pb3))]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate aligned-editor-canvas% () (parent frame) (editor pb1))])
  
  (send pb1 insert es2)
  (send pb2 insert es3)
  
  (test equal?
   (pasteboard-parent pb1)
   canvas)
  
  (test equal?
   (pasteboard-parent pb2)
   es2)
  
  (test equal?
   (pasteboard-parent pb3)
   es3)
  )

(let*
    ([pb1 (instantiate vertical-pasteboard% ())]
     [pb2 (instantiate horizontal-pasteboard% ())]
     [pb3 (instantiate vertical-pasteboard% ())]
     [es2 (instantiate aligned-editor-snip% () (editor pb2))]
     [es3 (instantiate aligned-editor-snip% () (editor pb3))]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate aligned-editor-canvas% () (parent frame) (editor pb1))])
  
  (send pb1 insert es2)
  (send pb1 insert es3)
  
  (test equal?
   (pasteboard-parent pb1)
   canvas)
  
  (test equal?
   (pasteboard-parent pb2)
   es2)
  
  (test equal?
   (pasteboard-parent pb3)
   es3)
  )
(tests-done)
