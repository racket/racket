(module geometry-managed-pasteboard mzscheme
  
  (require
   mzlib/class
   mzlib/contract
   mzlib/list
   mzlib/etc
   mzlib/match
   mred
   "aligned-editor-container.rkt"
   "interface.rkt"
   "alignment.rkt"
   "snip-lib.rkt"
   "pasteboard-lib.rkt")
  
  (provide/contract (make-aligned-pasteboard ((symbols 'vertical 'horizontal) . -> . class?)))
  
  ;;  mixin to add geometry management to pasteboard with the give type of alignement
  (define (make-aligned-pasteboard type)
    (class* pasteboard% (aligned-pasteboard<%>)
      
      (inherit resize move-to find-first-snip refresh-delayed?
               begin-edit-sequence end-edit-sequence is-locked? lock)
      
      (field
       [needs-realign? false]
       [ignore-resizing? false]
       [alloted-width false]
       [alloted-height false]
       [aligned-min-width 0]
       [aligned-min-height 0]
       [aligned-rects empty])
      
      ;; get-aligned-min-width (-> number?)
      ;; the aligned-min-width of the pasteboard
      (define/public (get-aligned-min-width) aligned-min-width)
      
      ;; get-aligned-min-height (-> number?)
      ;; the aligned-min-height of the pasteboard
      (define/public (get-aligned-min-height) aligned-min-height)
      
      (define/public (set-aligned-min-sizes)
        (dynamic-let ([ignore-resizing? true])
          (for-each-snip
           (lambda (s)
             (if (is-a? s aligned-editor-snip%)
                 (send s set-aligned-min-sizes)))
           (find-first-snip))
          (set!-values (aligned-min-width aligned-min-height)
                       (get-aligned-min-sizes type (find-first-snip)))))
      
      ;; set-algined-min-sizes (-> void?)
      ;; set the aligned min width and height of the pasteboard based on its children snips
      (inherit in-edit-sequence?)
      (define/public (aligned-min-sizes-invalid)
        ;; This in-edit-sequence? is not sound. It causes me to percollate invalidation
        ;; up the spin of my tree even when it is not visible (which refresh-delayed?
        ;; checks for. However, for some types of refreshed-delayed? blocks, like a
        ;; parent editor's edit-sequence, I have not yet figured out a way to reshedule
        ;; an alignment. With in-edit-sequence? blocking, I know I'll always get the
        ;; after-edit-sequence call where I can invoke alignment.
        (if (in-edit-sequence?) ;(refresh-delayed?)
            (set! needs-realign? true)
            (begin
              (set! needs-realign? false)
              (set!-values (aligned-min-width aligned-min-height)
                           (get-aligned-min-sizes type (find-first-snip)))
              (let ([parent (pasteboard-parent this)])
                (when parent (send parent aligned-min-sizes-invalid))))))
      
      ;; realign (case-> (-> void?) (positive? positive? . -> . void?))
      ;; called by the parent to realign the pasteboard's children
      (define/public (realign width height)
        (set! alloted-width width)
        (set! alloted-height height)
        (realign-to-alloted))
      
      ;; realign-to-alloted (-> void?)
      ;; realign the snips to fill the alloted width and height
      (define/public (realign-to-alloted)
        (when (and alloted-width alloted-height)
          (when (not (and (positive? alloted-width) (positive? alloted-height)))
            (error "allotted width or height is not positive"))
          (dynamic-let ([ignore-resizing? true])
            (let* ([first-snip (find-first-snip)]
                   [aligned-rects
                    (align type alloted-width alloted-height
                           (map-snip build-rect first-snip))])
              (begin-edit-sequence)
              (let ([was-locked? (is-locked?)])
                (lock false)
                (for-each-snip move/resize first-snip aligned-rects)
                (lock was-locked?))
              (end-edit-sequence)))))
      
      ;;move/resize (snip-pos? rect? . -> . void?)
      ;;moves and resizes the snips with in pasteboard
      (define move/resize
        (match-lambda*
          [(snip ($ rect
                    ($ dim x width stretchable-width?)
                    ($ dim y height stretchable-height?)))
           (move-to snip x y)
           (when (is-a? snip stretchable-snip<%>)
             (send snip stretch width height))]))
      
      ;;;;;;;;;;
      ;; Events
      
      ;; after-insert ((is-a?/c snip%) (is-a?/c snip%) number? number? . -> . void?)
      ;; called after a snip is inserted to the pasteboard
      (define/augment (after-insert snip before x y)
        (aligned-min-sizes-invalid)
        (inner (void) after-insert snip before x y))
      
      ;; after-delete ((is-a?/c snip%) . -> . void?)
      ;; called after a snip is deleted from the pasteboard%
      (define/augment (after-delete snip)
        (aligned-min-sizes-invalid)
        (inner (void) after-delete snip))
      
      ; after-reorder ((is-a?/c snip%) (is-a?/c snip%) boolean? . -> . void?)
      ;; called after a snip is moved in the front to back snip order
      (define/augment (after-reorder snip to-snip before?)
        (realign-to-alloted)
        (inner (void) after-reorder snip to-snip before?))
      
      ;; resized ((is-a?/c snip%) . -> . void?)
      ;; called when a snip inside the editor is resized
      (define/override (resized snip redraw-now?)
        (super resized snip redraw-now?)
        (unless ignore-resizing?
          (aligned-min-sizes-invalid)))
      
      ;; after-edit-sequence (-> void?)
      ;; called after an edit-sequence ends
      (define/augment (after-edit-sequence)
        (when needs-realign? (aligned-min-sizes-invalid))
        (inner (void) after-edit-sequence))
      
      (super-new)))
  
  ;; build-rect ((is-a?/c snip%) . -> . rect?)
  ;; makes a new default rect out of a snip
  (define (build-rect snip)
    (make-rect
     (make-dim 0 (snip-min-width snip) (stretchable-width? snip))
     (make-dim 0 (snip-min-height snip) (stretchable-height? snip))))
  
  ;; get-aligned-min-sizes (((symbols 'horizontal vertical) (is-a?/c snip%)) . ->* . (number? number?))
  ;; calculate the aligned min sizes for the pasteboard containing the given snips
  (define (get-aligned-min-sizes type init-snip)
    (let-values ([(x-func y-func)
                  (if (symbol=? type 'horizontal)
                      (values + max)
                      (values max +))])
      (let loop ([snip init-snip]
                 [width 0]
                 [height 0])
        (cond
          [(boolean? snip) (values width height)]
          [else
           (loop (send snip next)
                 (x-func (snip-min-width snip) width)
                 (y-func (snip-min-height snip) height))]))))
  
  ;; dynamic-let is just like fluid-let but is less expensive and not safe over continuations
  (define-syntax (dynamic-let stx)
    (syntax-case stx ()
      [(_ ((x y) ...) body body2 ...)
       (andmap identifier? (syntax-e #'(x ...)))
       (with-syntax ([(old-x ...) (generate-temporaries #'(x ...))])
         #'(let ((old-x x) ...)
             (begin
               (set! x y) ...
               (begin0
                 (begin
                   body
                   body2 ...)
                 (set! x old-x) ...))))]))
  )
