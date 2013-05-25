(module snip-wrapper mzscheme
  
  (require
   mzlib/etc
   mzlib/class
   mred
   "interface.rkt"
   (prefix sl: "snip-lib.rkt")
   "dllist.rkt")
  
  (provide control-style
	   snip-wrapper%)

  (define control-style
    (let ([delta (make-object style-delta%)])
      (send delta set-family 'system)
      (send delta set-delta 'change-size (send normal-control-font get-point-size))
      (send the-style-list find-or-create-style 
	    (send the-style-list basic-style)
	    delta)))
    
  (define snip-wrapper%
    (class* dllist% (alignment<%>)
      (init-field parent snip)
      (field [pasteboard (send parent get-pasteboard)]
             [show? true]
             [snip-is-inserted? false]
             [min-width 0]
             [min-height 0])
      
      ;;;;;;;;;;
      ;; alignment<%>
      
      #;(-> alignment-parent<%>)
      ;; The parent of this alignment
      (define/public (get-parent) parent)
      
      #;(-> void?)
      ;; Tells the alignment that its sizes should be calculated
      (define/public (set-min-sizes)
        (if snip-is-inserted?
            (begin (set! min-width (sl:snip-min-width snip))
                   (set! min-height (sl:snip-min-height snip)))
            (begin (set! min-width 0)
                   (set! min-height 0))))
      
      #;(nonnegative? nonnegative? nonnegative? nonnegative? . -> . void?)
      ;; Tells the alignment to align its children on the pasteboard in the given rectangle
      (define/public (align x y w h)
        (send pasteboard move-to snip x y)
        (when (is-a? snip stretchable-snip<%>)
          (send snip stretch w h)))
      
      #;(-> nonnegative?)
      ;; The minimum width this alignment must be.
      (define/public (get-min-width) min-width)
      
      #;(-> nonnegative?)
      ;; The minimum height this alignment must be.
      (define/public (get-min-height) min-height)
      
      #;(case-> (-> boolean?) (boolean? . -> . void?))
      ;; True if the alignment can be stretched in the x dimension
      (define/public (stretchable-width)
        (sl:stretchable-width? snip))
      
      #;(case-> (-> boolean?) (boolean? . -> . void?))
      ;; True if the alignment can be stretched in the y dimension
      (define/public (stretchable-height)
        (sl:stretchable-height? snip))
      
      #;(boolean? . -> . void?)
      ;; Tells the alignment that its show state is the given value
      ;; and it should show or hide its children accordingly.
      (define/public (show bool)
        (set! show? bool)
        (show/hide show?))
      
      #;(boolean? . -> . void)
      ;; Tells the alignment to show or hide its children
      (define/public (show/hide bool)
        (send pasteboard lock-alignment true)
        (unless (boolean=? bool snip-is-inserted?)
          (if bool
              (begin (send pasteboard insert-aligned-snip snip)
                     (set! snip-is-inserted? true))
              (begin (send pasteboard release-aligned-snip snip)
                     (set! snip-is-inserted? false))))
        (send pasteboard realign)
        (send pasteboard lock-alignment false))
      
      (super-new)
      (send parent add-child this)
      (show/hide (and show? (send parent is-shown?)))))
  )
