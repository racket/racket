(module verthoriz-alignment mzscheme
  
  (require
   mzlib/class
   mzlib/etc
   mzlib/list
   mzlib/match
   (prefix a: "alignment.rkt")
   "interface.rkt"
   "alignment-helpers.rkt"
   "dllist.rkt")
   
  (provide
   horizontal-alignment%
   vertical-alignment%)
  
  ;;;;;;;;;;
  ;; alignment
    
  (define (vert/horiz-alignment type)
    (class* dllist% (alignment<%> alignment-parent<%>)
      
      (init-field parent [show? true] [after #f])
      (init [stretchable-width true]
            [stretchable-height true])
      
      (field
       [pasteboard (send parent get-pasteboard)]
       [min-width 0]
       [min-height 0]
       [stretchable-width? stretchable-width]
       [stretchable-height? stretchable-height])
      
      (field
       [head (new head%)]
       [tail (new tail%)])
      
      (send head next tail)
      (send tail prev head)
      
      ;;;;;;;;;;
      ;; alignment<%>
      
      #;(-> alignment-parent<%>)
      ;; The parent of this alignment
      (define/public (get-parent) parent)
      
      #;(-> void?)
      ;; Tells the alignment that its sizes should be calculated
      (define/public (set-min-sizes)
        (when show?
          (send head for-each
                (lambda (child)
                  (send child set-min-sizes)))
          (let-values ([(x-accum y-accum)
                        (if (symbol=? type 'vertical)
                            (values vacuous-max +)
                            (values + vacuous-max))])
            (set! min-width
                  (apply x-accum
                         (send head map-to-list
                               (lambda (c) (send c get-min-width)))))
            (set! min-height
                  (apply y-accum
                         (send head map-to-list
                               (lambda (c) (send c get-min-height))))))))
      
      #;(nonnegative? nonnegative? nonnegative? nonnegative? . -> . void?)
      ;; Tells the alignment to align its children on the pasteboard in the given rectangle
      (define/public (align x-offset y-offset width height)
          
          (define move/resize
            (match-lambda*
              [(child ($ a:rect ($ a:dim x w _) ($ a:dim y h _)))
               (send child align (+ x x-offset) (+ y y-offset) w h)]))
          
          (when (is-shown?)
            (send head for-each
                  move/resize
                  (a:align type width height
                           (send head map-to-list build-rect)))))
      
      #;(-> nonnegative?)
      ;; The minimum width this alignment must be
      (define/public (get-min-width)
        (if (is-shown?) min-width 0))
      
      #;(-> nonnegative?)
      ;; The minimum height this alignment must be
      (define/public (get-min-height)
        (if (is-shown?) min-height 0))
      
      #;(case-> (-> boolean?) (boolean? . -> . void?))
      ;; True if the alignment can be stretched in the x dimension
      (public [stretchable-width-method stretchable-width])
      (define stretchable-width-method
        (case-lambda
          [() stretchable-width?]
          [(value) (set! stretchable-width? value)]))
      
      #;(case-> (-> boolean?) (boolean? . -> . void?))
      ;; True if the alignment can be stretched in the y dimension
      (public [stretchable-height-method stretchable-height])
      (define stretchable-height-method
        (case-lambda
          [() stretchable-height?]
          [(value) (set! stretchable-height? value)]))
      
      #;(boolean? . -> . void)
      ;; Tells the alignment to show or hide its children
      (define/public (show/hide bool)
        (when show? (show/hide-children bool)))
      
      #;(boolean? . -> . void)
      ;; Tells the alignment that its show state is the given value
      ;; and it should show or hide its children accordingly.
      (define/public (show bool)
        (set! show? bool)
        (when (send parent is-shown?)
          (show/hide-children bool)))
      
      #;(-> (is-a?/c pasteboard%))
      ;; The pasteboard that this alignment is being displayed to
      (define/public (get-pasteboard) pasteboard)
      
      #;(((is-a?/c alignment<%>)) ((union (is-a?/c alignment<%>) false?)) . opt-> . void?)
      ;; Add the given alignment as a child before the existing child
      (define/public add-child
        (opt-lambda (child (after #f))
          (define (link p item n)
            (send p next child)
            (send child prev p)
            (send n prev child)
            (send child next n))
          (if after
              (link after child (send after next))
              (link (send tail prev) child tail))))
      
      #;((is-a?/c alignment<%>) . -> . void?)
      ;; Deletes a child from the alignments
      (define/public (delete-child child)
        (send child show/hide false)
        (let ([p (send child prev)]
              [n (send child next)])
          (send p next n)
          (send n prev p)))
      
      #;(-> boolean?)
      ;; True if the alignment is being shown (accounting for its parent being shown)
      (define/public (is-shown?)
        (and show? (send parent is-shown?)))
      
      ;;;;;;;;;;
      ;; helpers
      
      #;(boolean? . -> . void?)
      ;; Shows or hides the children
      (define/private (show/hide-children bool)
        (send pasteboard lock-alignment true)
        (send head for-each (lambda (c) (send c show/hide bool)))
        (send pasteboard lock-alignment false))
      
      (super-new)
      (send parent add-child this after)))
  
  (define vertical-alignment% (vert/horiz-alignment 'vertical))
  (define horizontal-alignment% (vert/horiz-alignment 'horizontal))
  
  #;((is-a?/c alignment%) . -> . rect?)
  ;; makes a new default rect out of an alignment
  (define (build-rect item)
    (a:make-rect
     (a:make-dim 0 (max 0 (send item get-min-width)) (send item stretchable-width))
     (a:make-dim 0 (max 0 (send item get-min-height)) (send item stretchable-height))))
  )
