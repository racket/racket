(module interface mzscheme
  
  (require mzlib/class)
  
  (provide stretchable-snip<%>
           alignment<%>
           alignment-parent<%>
           dllist<%>)
  
  (define dllist<%>
    (interface ()
      next
      prev
      for-each
      map-to-list))
  
  (define alignment<%>
    (interface (dllist<%>)
      
      #;(-> alignment-parent<%>)
      ;; The parent of this alignment
      get-parent
      
      #;(-> void?)
      ;; Tells the alignment that its sizes should be calculated
      set-min-sizes
      
      #;(nonnegative? nonnegative? nonnegative? nonnegative? . -> . void?)
      ;; Tells the alignment to align its children on the pasteboard in the given rectangle
      align
      
      #;(-> nonnegative?)
      ;; The minimum width this alignment must be
      get-min-width
      
      #;(-> nonnegative?)
      ;; The minimum height this alignment must be
      get-min-height
      
      #;(case-> (-> boolean?) (boolean? . -> . void?))
      ;; True if the alignment can be stretched in the x dimension
      stretchable-width
      
      #;(case-> (-> boolean?) (boolean? . -> . void?))
      ;; True if the alignment can be stretched in the y dimension
      stretchable-height
      
      #;(boolean? . -> . void)
      ;; Tells the alignment to show or hide its children
      show/hide
      
      #;(boolean? . -> . void)
      ;; Tells the alignment that its show state is the given value
      ;; and it should show or hide its children accordingly.
      show
      ))
  
  (define alignment-parent<%>
    (interface ()
      #;(-> (is-a?/c pasteboard%))
      ;; The pasteboard that this alignment is being displayed to
      get-pasteboard
      
      #;(((is-a?/c alignment<%>)) ((union (is-a?/c alignment<%>) false?)) . opt-> . void?)
      ;; Add the given alignment as a child before the existing child
      add-child
      
      #;((is-a?/c alignment<%>) . -> . void?)
      ;; Deletes a child from the alignments
      delete-child
      
      #;(-> boolean?)
      ;; True if the alignment is being shown (accounting for its parent being shown)
      is-shown?
      ))
  
  #| the interface that must be implemented by a class to be inserted into an
     aligned-pasteboard<%> and be stretched and shrunk according to the geometry managment.
  
     note: any snip may be insert... those
     that do not implement stretchable-snip<%> will simply not be stretched.
  |#
  (define stretchable-snip<%>
    (interface ()
      #;(positive? positive? . ->  . void?)
      ;; called by the parent editor to stretch the snip to an specific size
      stretch
      
      #;(-> positive?)
      ;; get the minimum width of the snip
      get-aligned-min-width
      
      #;(-> positive?)
      ;; get the minmum height of the snip
      get-aligned-min-height
      
      #;(case-> (boolean . -> . void?) (-> boolean?))
      ;; get or set the stretchablity of the pasteboards width
      stretchable-width
      
      #;(case-> (boolean . -> . void?) (-> boolean?))
      ;; get or set the stretchablity of the pasteboards height
      stretchable-height
      ))
  )
