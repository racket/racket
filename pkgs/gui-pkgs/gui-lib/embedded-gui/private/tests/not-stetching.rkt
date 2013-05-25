(module not-stetching mzscheme
  
  (require
   mike-lib/debug
   mzlib/class
   mzlib/etc
   mred
   embedded-gui/aligned-pasteboard
   embedded-gui/stretchable-editor-snip
   embedded-gui/verthoriz-alignment)
  
  (define traced-ses%
    #;(override/trace stretchable-editor-snip%
                    (stretch
                     get-aligned-min-width
                     get-aligned-min-height
                     stretchable-width
                     stretchable-height)))
  
  (define f (new frame% (label "f") (width 400) (height 400)))
  (define e (new text%))
  (define c (new editor-canvas% (parent f) (editor e)))
  (define main (new aligned-pasteboard%))
  (define j (new editor-snip% (editor main)))
  (define line (new horizontal-alignment% (parent main)))
  (define ses (new traced-ses% (editor (new text%))))
  (send line add ses)
  (send main add (make-object string-snip% "super duper very long snip"))
  (send e insert j)
  (send f show true)
  )
