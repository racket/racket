(module grey-editor mzscheme
  
  (provide grey-editor-snip-mixin grey-editor-mixin)
  
  (require
   mred
   mzlib/class
   framework)
  
  (define *disable-color* (make-object color% 235 235 255))
  
  (define grey-editor-snip-mixin
    (mixin ((class->interface editor-snip%)) ()
      (inherit get-admin get-inset)
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)]
              [admin (get-admin)]
              [left-inset (box 0)]
              [top-inset (box 0)]
              [right-inset (box 0)]
              [bottom-inset (box 0)]
              [xb (box 0)]
              [yb (box 0)]
              [wb (box 0)]
              [hb (box 0)])
          (when admin
            (send admin get-view xb yb wb hb this)
            (get-inset left-inset top-inset right-inset bottom-inset)
            (send dc set-pen (send the-pen-list find-or-create-pen *disable-color* 1 'solid))
            (send dc set-brush (send the-brush-list find-or-create-brush *disable-color* 'solid))
            (send dc draw-rectangle
                  (+ x (unbox xb) (unbox left-inset))
                  (+ y (unbox yb) (unbox top-inset))
                  (max 0 (- (unbox wb) (+ (unbox left-inset) (unbox right-inset))))
                  (max 0 (- (unbox hb) (+ (unbox top-inset) (unbox bottom-inset)))))
            (send dc set-pen old-pen)
            (send dc set-brush old-brush)))
        (super draw dc x y left top right bottom dx dy draw-caret))
      (super-new)))
  
  (define grey-editor-mixin
    (mixin (editor<%>) ()
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (when before?
          (let ([old-pen (send dc get-pen)]
                [old-brush (send dc get-brush)])
            (send dc set-pen (send the-pen-list find-or-create-pen *disable-color* 1 'solid))
            (send dc set-brush (send the-brush-list find-or-create-brush *disable-color* 'solid))
            (send dc draw-rectangle (+ left dx) (+ top dy) (+ right dx) (+ bottom dy))
            (send dc set-pen old-pen)
            (send dc set-brush old-brush)))
        (super on-paint before? dc left top right bottom dx dy draw-caret))
      (super-new))))
