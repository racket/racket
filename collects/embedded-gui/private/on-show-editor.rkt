(module on-show-editor mzscheme
  (define (writeln x) (write x) (newline))
  (require
   mzlib/class
   mzlib/etc
   mred
   framework
   "snip-lib.rkt")
  
  (provide
   on-show-editor<%>
   on-show-editor-canvas-mixin
   on-show-editor-snip-mixin)
  
  (define on-show-editor<%>
    (interface ()
      #;(-> void)
      ;; Called when the editor is shown.
      on-show))
  
  (define on-show-editor-snip-mixin
    (mixin ((class->interface editor-snip%)) ()
      (super-new)))
  
  (define on-show-editor-canvas-mixin
    (mixin ((class->interface editor-canvas%)) ()
      (inherit get-editor)
      #;(number? number? . -> . void)
      ;; called when the canvas's parent size changes
      (define/override (on-size width height)
        (super on-size width height)
        (send (get-editor) on-show))
      (super-new)))
  )
