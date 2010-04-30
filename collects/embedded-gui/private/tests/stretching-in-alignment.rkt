(module stretching-in-alignment mzscheme
    
  (require
   mred
   mzlib/etc
   mzlib/class
   mrlib/private/aligned-pasteboard/interface
   mike-lib/debug
   mzlib/list
   mzlib/match
   (prefix a: "alignment.ss")
   mrlib/click-forwarding-editor
   mrlib/private/aligned-pasteboard/snip-lib
   embedded-gui/aligned-pasteboard
   embedded-gui/stretchable-editor-snip)
  
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
