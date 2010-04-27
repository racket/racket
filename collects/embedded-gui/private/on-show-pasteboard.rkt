(module on-show-pasteboard mzscheme
  
  (require
   mzlib/class
   mzlib/etc
   mred)
  
  (provide
   on-show-pasteboard%
   on-show-pasteboard-mixin)
  
  (define (on-show-pasteboard-mixin super%)
    (class super%
      (field [shown? false])
      #|
      (define/override (refresh x y w h d-c)
        (super refresh x y (max w 0) (max h 0) d-c)
        (unless shown?
          (set! shown? true)
          (on-show)))
      |#
      #|
      (define/override (get-extent w h)
        (super get-extent w h)
        (unless shown?
          (set! shown? true)
          (on-show)))
      |#
      (define/public (showing?) shown?)
      (define/public (on-show) (void))
      (super-new)))
  
  (define on-show-pasteboard%
    (on-show-pasteboard-mixin
     pasteboard%))
  
  #|
  (define f (new frame% (label "f") (width 400) (height 400)))
  (send f show true)
  (define e (new pasteboard%))
  (define c (new editor-canvas% (editor e) (parent f)))
  (define pb (new on-show-pasteboard%))
  (define es (new editor-snip% (editor pb)))
  (not (send pb showing?))
  (send e insert es)
  (send pb showing?)
  (send e remove es)
  (not (send pb showing?))
  |#
  )
