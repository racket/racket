#| Note: It might be a good idea to override insert with an error so that people don't
   insert or delete from the pasteboard without using the alignment<%>. Then the alignments
   could go through a different interface for inserting the snips that would call
   super-insert.
|#

(module aligned-pasteboard mzscheme

  (provide aligned-pasteboard%)

  (require
   mzlib/class
   mred
   mzlib/etc
   mrlib/click-forwarding-editor
   "on-show-pasteboard.rkt"
   "really-resized-pasteboard.rkt"
   "interface.rkt"
   "locked-pasteboard.rkt"
   "suppress-modify-editor.rkt"
   "on-show-editor.rkt")

  (define aligned-pasteboard%
    (class* (click-forwarding-editor-mixin
             (suppress-modify-editor-mixin
              (locked-pasteboard-mixin
               (really-resized-pasteboard-mixin pasteboard%))))
      (alignment-parent<%> on-show-editor<%>)
      
      (inherit begin-edit-sequence end-edit-sequence
               get-max-view-size refresh-delayed?)
      
      (field
       [alignment false]
       [lock-alignment-depth 0]
       [needs-alignment? false])
      
      ;;;;;;;;;;
      ;; insert/delete
      
      (rename-super [super-insert insert] [super-release-snip release-snip])
      (define/override (insert . args) (void))
      (define/override (release-snip snip) (void))
      
      #;((is-a?/c snip%) . -> . void)
      ;; The insert that can be called from snip-wrappers to install a snip in the pasteboard
      (define/public (insert-aligned-snip snip)
        #;(lock-alignment true)
        (super-insert snip)
        #;(realign)
        #;(lock-alignment false))
      
      #;((is-a?/c snip%) . -> . void)
      ;; The release-snip that can be called from snip-wrappers to clear a snip
      (define/public (release-aligned-snip snip)
        #;(lock-alignment true)
        (super-release-snip snip)
        #;(realign)
        #;(lock-alignment false))
      
      ;;;;;;;;;;
      ;; alignment-parent<%>
      
      #;(-> (is-a?/c pasteboard%))
      ;; The pasteboard that this alignment is being displayed to
      (define/public (get-pasteboard) this)
      
      #;(((is-a?/c alignment<%>)) ((union (is-a?/c alignment<%>) false?)) . opt-> . void?)
      ;; Add the given alignment as a child before the existing child
      (define/public add-child
        (opt-lambda (child (after #f))
          (if alignment
              (error 'add-child "There may be only one alignment<%> of a pasteboard")
              (set! alignment child))))
      
      #;((is-a?/c alignment<%>) . -> . void?)
      ;; Deletes a child from the alignments
      (define/public (delete-child child)
        (if alignment
            (if (eq? child alignment)
                (set! alignment false)
                (error 'delete-child "Child not found"))
            (error 'delete-child "No children")))
      
      #;(-> (listof (is-a?/c alignment<%>)))
      ;; A list of the children of this alignment parent
      (define/public (get-children) (list alignment))
            
      #;(-> boolean?)
      ;; True if the alignment is being shown (accounting for its parent being shown)
      ;; NOTE: Pasteboards are always shown and have no show/hide state.
      (define/public (is-shown?) true)
      
      #;((is-a?/c snip%) . -> . void?)
      ;; Called when a snip in the pasteboard changes its size
      ;; Overriden because the layout will change when a snip gets bigger.
      (define/override (really-resized snip)
        (super really-resized snip)
        (realign))
      
      #;(-> void)
      ;; Called when the pasteboard is first shown.
      ;; Overriden because I need to know when the snips have their size to lay them out.
      (define/public (on-show)
        (realign))
      
      #;(boolean? . -> . void?)
      ;; Locks the pasteboard so that all alignment requests are delayed until after it's done.
      ;; STATUS: I can still observe inserts of the interactions when I paste an interactions box.
      ;; I need to figure out why these edit-sequences are not hiding them.
      (define/public (lock-alignment lock?)
        (set! lock-alignment-depth ((if lock? add1 sub1) lock-alignment-depth))
        (case lock-alignment-depth
          [(0) (when needs-alignment? (realign))
               (unless lock? (end-edit-sequence))]
          [(1) (when lock? (begin-edit-sequence))]
          [else (void)]))
      
      #;(-> void?)
      ;; Realigns the snips in the pasteboard according to the alignment tree.
      (define/public (realign)
        (if (zero? lock-alignment-depth)
            (fluid-let ([lock-alignment-depth (add1 lock-alignment-depth)])
              (begin-edit-sequence)
              (send alignment set-min-sizes)
              (let ([width (send alignment get-min-width)]
                    [height (send alignment get-min-height)])
                (unless (or (zero? width) (zero? height))
                  (send alignment align 0 0 width height)
                  (set! needs-alignment? false)))
              (end-edit-sequence))
            (set! needs-alignment? true)))
      
      (super-new)))
  )
