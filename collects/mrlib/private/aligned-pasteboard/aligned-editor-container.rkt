(module aligned-editor-container mzscheme
  
  (require
   mzlib/class
   mred
   mzlib/etc
   mzlib/list
   "snip-lib.rkt"
   "interface.rkt"
   "constants.rkt")

  (provide
   aligned-editor-canvas%
   aligned-editor-snip%)

  ;; a canvas that can contain an aligned-pasteboard<%>
  ;; STATUS: When both min-width and min-height change the size of the canvas
  ;;         I might be getting two on-size method invocations inside
  ;;         set-aligned-min-sizes.
  ;;           Also, I might not need to call realign-to-alloted in
  ;;         set-aligned-min-sizes of the canvas because realign is called from
  ;;         within on-size. This is true if and only if realignment needs to
  ;;         be called only when the canvas size changes.
  (define aligned-editor-canvas%
    (class* editor-canvas% (aligned-pasteboard-parent<%>)
      (inherit get-editor get-size min-width min-height)
      (init-field (style empty))
      
      (field
       (width-diff 0)
       (height-diff 0))
      
      ;; set-aligned-min-size (-> (void))
      ;; sets the aligned min width and height of all aligned children
      (define/public (aligned-min-sizes-invalid)
        (let ([editor (get-editor)])
          (when (memq 'no-hscroll style)
            (min-width
             (+ (inexact->exact
                 (send editor get-aligned-min-width))
                machenrys-constant width-diff)))
          (when (memq 'no-vscroll style)
            (min-height
             (+ (inexact->exact
                 (send editor get-aligned-min-height))
                machenrys-constant height-diff)))
          ;; I might need to call realign not realign-to-alloted, but with what values?
          (send editor realign-to-alloted)))
      
      ;; on-size (number? number? . -> . (void))
      ;; called when the canvas's parent size changes
      (define/override (on-size width height)
        (super on-size width height)
        (let ([w (- width width-diff machenrys-constant)]
              [h (- height height-diff machenrys-constant)])
          (when (and (positive? w) (positive? h))
            (send* (get-editor)
              (set-aligned-min-sizes)
              (realign w h)))))
      
      ;; calc-view-client-diff (-> (void))
      ;; calculates and sets the difference between client-size and view-size of the editor
      (define/private (calc-view-client-diff)
        (let-values ([(width height) (get-size)])
          (let ([view-width (box 0)]
                [view-height (box 0)])
            (send (get-editor) get-view-size
                  view-width view-height)
            (set! width-diff
                  (- width
                     (inexact->exact
                      (unbox view-width))))
            (set! height-diff
                  (- height
                     (inexact->exact
                      (unbox view-height)))))))
      
      (super-new (style style))
      (calc-view-client-diff)))
  
  ;; a snip that can contain an aligned-pasteboard<%> and also be stretched within an aligned-pasteboard<%>
  (define aligned-editor-snip%
    (class* editor-snip% (aligned-pasteboard-parent<%> stretchable-snip<%>)
      (inherit get-editor get-margin set-min-width set-min-height)
      
      (init
       (stretchable-width true)
       (stretchable-height true))
      
      (field
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
      
      ;; (positive? positive? . -> . void?)
      ;; called to resize the snip to a given size without effecting its alignd-min-sizes
      ;; STATUS: Do I need to override resize and have it set the aligned-min-sizes?
      (inherit resize)
      (define/public (stretch width height)
        (resize width height)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)])
          (get-margin left top right bottom)
          (let ([w (- width (unbox left) (unbox right))]
                [h (- height (unbox top) (unbox bottom))])
            (when (and (positive? w) (positive? h))
              (send (get-editor) realign w h)))))
      
      ;; get-aligned-min-width (-> number?)
      ;; the minimum width of the snip based on the children
      (define/public (get-aligned-min-width)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)])
          (get-margin left top right bottom)
          (+ (unbox left)
             (unbox right)
             (send (get-editor) get-aligned-min-width)
             machenrys-constant)))
      
      ;; get-aligned-min-height (-> number?)
      ;; the minimum height of the snip based on the children
      (define/public (get-aligned-min-height)
        (let ([left (box 0)]
              [top (box 0)]
              [right (box 0)]
              [bottom (box 0)])
          (get-margin left top right bottom)
          (+ (unbox top)
             (unbox bottom)
             (send (get-editor) get-aligned-min-height)
             machenrys-constant)))
      
      ;; (-> void?)
      ;; sets the aligned-min-sizes of all the editors and snips in this snip
      (define/public (set-aligned-min-sizes)
        (send (get-editor) set-aligned-min-sizes))
      
      ;; (-> void?)
      ;; calculates and stores the minimum height and width of the snip
      ;; note: more efficient to check for parent ahead of time and not
      ;; calculate the margins when I don't have one.
      (define/public (aligned-min-sizes-invalid)
        (let ([parent (snip-parent this)])
          (cond
            [(not parent) (void)]
            [(is-a? parent aligned-pasteboard<%>)
             (send parent aligned-min-sizes-invalid)]
            [else (align-to-min)])))
      
      ;; This code is needed to probe the tree of editors for their real sizes when they
      ;; finally know them. This happens when the top level snip gets an admin.
      (define/override (set-admin admin)
        (super set-admin admin)
        (let ([parent (snip-parent this)])
          (when (and parent (not (is-a? parent aligned-pasteboard<%>)))
            (set-aligned-min-sizes)
            (align-to-min))))
      
      (define (align-to-min)
        ;; Note: Not setting the min-width might improve efficientcy and
        ;;       may not be necessary since snips grow to the size of
        ;;       the things they contain. I'm going to try it so the
        ;;       following two lines are commented out.
        ;(set-min-width aligned-min-width)
        ;(set-min-height aligned-min-height)
        (let* ([ed (get-editor)]
               [w (send ed get-aligned-min-width)]
               [h (send ed get-aligned-min-height)])
          (when (and (positive? w) (positive? h))
            (send ed realign w h))))
      
      (super-new)))
  )
