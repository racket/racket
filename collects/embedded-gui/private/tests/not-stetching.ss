(module not-stetching mzscheme
  
  (require
   (lib "debug.ss" "mike-lib")
   (lib "class.ss")
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "aligned-pasteboard.ss" "embedded-gui")
   (lib "stretchable-editor-snip.ss" "embedded-gui")
   (lib "verthoriz-alignment.ss" "embedded-gui"))
  
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
