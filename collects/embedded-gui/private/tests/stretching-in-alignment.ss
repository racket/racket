(module stretching-in-alignment mzscheme
    
  (require
   mred
   mzlib/etc
   mzlib/class
   (lib "interface.ss" "mrlib" "private" "aligned-pasteboard")
   (lib "debug.ss" "mike-lib")
   mzlib/list
   mzlib/match
   (prefix a: "alignment.ss")
   (lib "click-forwarding-editor.ss" "mrlib")
   (lib "snip-lib.ss" "mrlib" "private" "aligned-pasteboard")
   (lib "aligned-pasteboard.ss" "embedded-gui")
   (lib "stretchable-editor-snip.ss" "embedded-gui"))
  
  (define f (new frame% (label "f") (width 400) (height 400)))
  (define e (new text%))
  (define c (new editor-canvas% (editor e) (parent f)))
  (define pb (new aligned-pasteboard%))
  (define es (new editor-snip% (editor pb)))
  (define ses (new (stretchable-editor-snip-mixin editor-snip%)
                   (editor (new text%))))
  (send* pb
    (add (new (stretchable-editor-snip-mixin editor-snip%)
              (editor (new text%))))
    (add (make-object string-snip% "This snip is very long")))
  (send e insert es)
  (send f show true)
  )
