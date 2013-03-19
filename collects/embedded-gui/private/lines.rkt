(module lines mzscheme
  
  (require
   mzlib/class
   mzlib/etc
   mred
   "snip-wrapper.rkt"
   "interface.rkt")
  
  (provide hline% vline%)
  
  (define hline%
    (class snip-wrapper%
      (super-new
       (snip (new hline-snip%)))))
  
  (define vline%
    (class snip-wrapper%
      (super-new
       (snip (new vline-snip%)))))
  
  (define (make-line-snip draw-line stretch-w stretch-h classname)
    (letrec ([c (class* snip% (stretchable-snip<%>)
               
               (field
                [width 0]
                [height 0])
               
               ;;;;;;;;;;
               ;; snip%
               
               #;(((is-a?/c dc<%>)
                   number?
                   number?)
                  ((union nonnegative? false/c)
                   (union nonnegative? false/c)
                   (union nonnegative? false/c)
                   (union nonnegative? false/c)
                   (union nonnegative? false/c)
                   (union nonnegative? false/c))
                  . opt-> .
                  void?)
               (define/override (get-extent dc x y w h descent space lspace rspace)
                 (super get-extent dc x y w h descent space lspace rspace)
                 (when w (set-box! w width))
                 (when h (set-box! h height)))
               
               #;((is-a?/c dc<%>)
                  number?
                  number?
                  number?
                  number?
                  number?
                  number?
                  number?
                  number?
                  (symbols no-caret show-inactive-caret show-caret))
               (define/override (draw dc x y left top right bottom dx dy draw-caret)
                 (super draw dc x y left top right bottom dx dy draw-caret)
                 (draw-line dc x y width height)
                 (void))
               
               ;;;;;;;;;;
               ;; stretchable-snip<%>
               
               #;(positive? positive? . ->  . void?)
               ;; called by the parent editor to stretch the snip to an specific size
               (define/public (stretch w h)
                 (set! width w)
                 (set! height h))
               
               #;(-> positive?)
               ;; get the minimum width of the snip
               ;; NOTE: Lines need a margin so they're not overwritten
               (define/public (get-aligned-min-width) 8)
               
               #;(-> positive?)
               ;; get the minmum height of the snip
               ;; NOTE: Lines need a margin so they're not overwritten
               (define/public (get-aligned-min-height) 8)
               
               #;(case-> (boolean . -> . void?) (-> boolean?))
               ;; get or set the stretchablity of the pasteboards width
               (define/public stretchable-width
                 (case-lambda
                   [(v) (error 'stretchable-width "Cannot set stretchable-width of a line-snip%")]
                   [() stretch-w]))
               
               #;(case-> (boolean . -> . void?) (-> boolean?))
               ;; get or set the stretchablity of the pasteboards height
               (define/public stretchable-height
                 (case-lambda
                   [(v) (error 'stretchable-height "Cannot set stretchable-height of a line-snip%")]
                   [() stretch-h]))
               
               (super-new)
               (inherit set-snipclass)
               (set-snipclass sc))]
             [sc (new
                  (class snip-class%
                    (define/override (read f)
                      (new c))
                    (super-new)))])
      (send sc set-classname classname)
      (send sc set-version 1)
      (send (get-the-snip-class-list) add sc)
      c))
                
  
  (define hline-snip%
     (make-line-snip
      (lambda (dc x y width height)
        (send dc draw-line x (+ y (/ height 2)) (+ x width) (+ y (/ height 2))))
      true
      false
      "make-hline-snip"))
    
  (define vline-snip%
     (make-line-snip
      (lambda (dc x y width height)
        (send dc draw-line (+ x (/ width 2)) y (+ x (/ width 2))  (+ y height)))
      false
      true
      "make-line-snip"))
  
  #|
  (require
   "verthoriz-alignment.rkt"
   "aligned-pasteboard.rkt"
   "snip-wrapper.rkt")
  
  (define f (new frame% (label "f") (height 500) (width 500)))
  (send f show true)
  (define p (new aligned-pasteboard%))
  (define c (new editor-canvas% (editor p) (parent f)))
  (define a1 (new vertical-alignment% (parent p)))
  (define a2 (new horizontal-alignment% (parent a1)))
  (new hline% (parent a1))
  (define a3 (new horizontal-alignment% (parent a1)))
  
  (new snip-wrapper%
       (snip (make-object string-snip% "One"))
       (parent a2))
  (new snip-wrapper%
       (snip (make-object string-snip% "Two"))
       (parent a2))
  (new snip-wrapper%
       (snip (make-object string-snip% "Three"))
       (parent a3))
  (new snip-wrapper%
       (snip (make-object string-snip% "Three"))
       (parent a3))
  |#
  )
