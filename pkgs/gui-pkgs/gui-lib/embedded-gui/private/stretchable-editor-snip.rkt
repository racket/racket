(module stretchable-editor-snip mzscheme
  
  (provide
   stretchable-editor-snip%
   stretchable-editor-snip-mixin)
  
  (require
   mzlib/class
   mred
   mzlib/etc
   
   "interface.rkt")
  
  (define (stretchable-editor-snip-mixin super%)
    (class* super% (stretchable-snip<%>)
      
      (init
       (stretchable-width true)
       (stretchable-height true))
      
      (field
       (aligned-min-width 0)
       (aligned-min-height 0)
       (stretchable-width-field stretchable-width)
       (stretchable-height-field stretchable-height))
      
      (public (stretchable-width-method stretchable-width)
              (stretchable-height-method stretchable-height))
      
      ;; stretchable-width (case-> (Boolean . -> . (void)) (-> Boolean))
      ;; get or set the stretchablity of the pasteboards width
      (define stretchable-width-method
        (case-lambda
          [(value) (set! stretchable-width-field value)]
          [() stretchable-width-field]))
      
      ;; stretchable-height (case-> (Boolean . -> .(void)) (-> Boolean))
      ;; get or set the stretchablity of the pasteboards height
      (define stretchable-height-method
        (case-lambda
          [(value) (set! stretchable-height-field value)]
          [() stretchable-height-field]))
      
      (define/public (get-aligned-min-width) aligned-min-width)
      (define/public (get-aligned-min-height) aligned-min-height)
      
      (inherit get-margin get-editor get-admin)
      (define/override (resize w h)
        (set! aligned-min-width w)
        (set! aligned-min-height h)
        (super-resize w h))
        
      (define/public (stretch w h)
        (super-resize w h))
      
      (define/override (get-extent dc x y w h descent space lspace rspace)
        (super get-extent dc x y w h descent space lspace rspace)
        (when (is-a? (get-editor) text%)
          (set-box! w (sub1 (unbox w))))
        (go))
      
      (define/override (set-min-width w)
        ;; account for margin !!!!!!
        (send (get-editor) set-min-width w))
      
      (define/override (set-min-height h)
        ;; account for margin !!!!!!
        (send (get-editor) set-min-height h))
      
      ;; NOTE: Can I make this not public? I don't think it
      ;; should be but it's been a while since I wrote this class.
      (define/public (super-resize w h)
        (let ((top (box 0))
              (bot (box 0))
              (lef (box 0))
              (rit (box 0)))
          (get-margin top bot lef rit)
          (let ((w (max (- w (unbox lef) (unbox rit)) 0))
                (h (max (- h (unbox top) (unbox bot)) 0))
                (e (get-editor))
                (a (get-admin)))
            ;; subtracting 1 from W seems to make it act more like the editor-snip
            ;; because the C code has a hack to sub1 to make it look better. I am not
            ;; sure if this change here is sound and works for every part of this
            ;; class.
            (if (> w aligned-min-width)
                (super set-min-width w)
                (super set-min-width 'none))
            (if (> h aligned-min-height)
                (super set-min-height h)
                (super set-min-height 'none))
            (when a (send a resized this #t)))))
      
      ;; call this from within get extent and use the values it produces by subtracting the
      ;; margin instead of calling the editors get-extent and adding the margin.
      (define (go)
        (let ([w (box 0)]
              [h (box 0)]
              (top (box 0))
              (bot (box 0))
              (lef (box 0))
              (rit (box 0)))
          (get-margin top bot lef rit)
          (send (get-editor) get-extent w h)
          (set! aligned-min-width (+ (unbox w) (unbox lef) (unbox rit)))
          (set! aligned-min-height (+ (unbox h) (unbox top) (unbox rit)))))
            
      (super-new)
      (inherit get-min-width get-min-height)
      (set-min-width (get-min-width))
      (set-min-height (get-min-height))))
  
  (define stretchable-editor-snip%
    (stretchable-editor-snip-mixin
     editor-snip%))
  )
