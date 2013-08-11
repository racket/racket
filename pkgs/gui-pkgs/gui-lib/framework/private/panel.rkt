#lang racket/base

(require racket/class
         racket/list
         racket/unit
         "sig.rkt"
         mred/mred-sig
         mrlib/switchable-button)
(provide panel@)

(define-unit panel@
  (import [prefix icon: framework:icon^]
          mred^)
  (export framework:panel^)
  (init-depend mred^)
  
  (define single<%> (interface (area-container<%>) active-child))
  (define single-mixin
    (mixin (area-container<%>) (single<%>)
      (inherit get-alignment change-children)
      (define/override (after-new-child c)
        (unless (is-a? c window<%>)
          
          ;; would like to remove the child here, waiting on a PR submitted
          ;; about change-children during after-new-child
          (change-children
           (λ (l)
             (remq c l)))
          
          (error 'single-mixin::after-new-child
                 "all children must implement window<%>, got ~e"
                 c))
        (if current-active-child
            (send c show #f)
            (set! current-active-child c)))
      [define/override (container-size l)
        (if (null? l)
            (values 0 0)
            (values (apply max (map car l)) (apply max (map cadr l))))]
      [define/override (place-children l width height)
        (let-values ([(h-align-spec v-align-spec) (get-alignment)])
          (let ([align
                 (λ (total-size spec item-size)
                   (floor
                    (case spec
                      [(center) (- (/ total-size 2) (/ item-size 2))]
                      [(left top) 0]
                      [(right bottom) (- total-size item-size)]
                      [else (error 'place-children
                                   "alignment spec is unknown ~a\n" spec)])))])
            (map (λ (l) 
                   (let*-values ([(min-width min-height h-stretch? v-stretch?)
                                  (apply values l)]
                                 [(x this-width)
                                  (if h-stretch?
                                      (values 0 width)
                                      (values (align width h-align-spec min-width)
                                              min-width))]
                                 [(y this-height)
                                  (if v-stretch?
                                      (values 0 height)
                                      (values (align height v-align-spec min-height)
                                              min-height))])
                     (list x y this-width this-height)))
                 l)))]
      
      (inherit get-children begin-container-sequence end-container-sequence)
      [define current-active-child #f]
      (define/public active-child
        (case-lambda
          [() current-active-child]
          [(x) 
           (unless (memq x (get-children))
             (error 'active-child "got a panel that is not a child: ~e" x))
           (unless (eq? x current-active-child)
             (begin-container-sequence)
             (for-each (λ (x) (send x show #f))
                       (get-children))
             (set! current-active-child x)
             (send current-active-child show #t)
             (end-container-sequence))]))
      (super-instantiate ())))
  
  (define single-window<%> (interface (single<%> window<%>)))
  (define single-window-mixin
    (mixin (single<%> window<%>) (single-window<%>)
      (inherit get-client-size get-size)
      [define/override container-size
        (λ (l)
          (let-values ([(super-width super-height) (super container-size l)]
                       [(client-width client-height) (get-client-size)]
                       [(window-width window-height) (get-size)]
                       [(calc-size)
                        (λ (super client window)
                          (+ super (max 0 (- window client))))])
            
            (values
             (calc-size super-width client-width window-width)
             (calc-size super-height client-height window-height))))]
      (super-new)))
  
  (define multi-view<%>
    (interface (area-container<%>)
      split-vertically
      split-horizontally
      collapse))
  
  (define multi-view-mixin
    (mixin (area-container<%>) (multi-view<%>) 
      (init-field parent editor)
      (public get-editor-canvas% get-vertical% get-horizontal%)
      [define get-editor-canvas%
        (λ ()
          editor-canvas%)]
      [define get-vertical%
        (λ ()
          vertical-panel%)]
      [define get-horizontal%
        (λ ()
          horizontal-panel%)]
      
      (define/private (split p%)
        (let ([canvas (send (send parent get-top-level-window) get-edit-target-window)]
              [ec% (get-editor-canvas%)])
          (when (and canvas
                     (is-a? canvas ec%)
                     (eq? (send canvas get-editor) editor))
            (let ([p (send canvas get-parent)])
              (send p change-children (λ (x) null))
              (let ([pc (make-object p% p)])
                (send (make-object ec% (make-object vertical-panel% pc) editor) focus)
                (make-object ec% (make-object vertical-panel% pc) editor))))))
      [define/public split-vertically
        (λ ()
          (split (get-vertical%)))]
      [define/public split-horizontally
        (λ ()
          (split (get-horizontal%)))]
      
      (define/public (collapse)
        (let ([canvas (send (send parent get-top-level-window) get-edit-target-window)]
              [ec% (get-editor-canvas%)])
          (when (and canvas
                     (is-a? canvas ec%)
                     (eq? (send canvas get-editor) editor))
            (let ([p (send canvas get-parent)])
              (if (eq? p this)
                  (bell)
                  (let* ([sp (send p get-parent)]
                         [p-to-remain (send sp get-parent)])
                    (send p-to-remain change-children (λ (x) null))
                    (send (make-object ec% p-to-remain editor) focus)))))))
      
      
      (super-instantiate () (parent parent))
      (make-object (get-editor-canvas%) this editor)))
  
  (define single% (single-window-mixin (single-mixin panel%)))
  (define single-pane% (single-mixin pane%))
  (define multi-view% (multi-view-mixin vertical-panel%))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; type gap = (make-gap number area<%> percentage number area<%> percentage)
  (define-struct gap (before before-dim before-percentage after after-dim after-percentage))
  
  ;; type percentage : (make-percentage number)
  (define-struct percentage (%) #:mutable)
  
  (define dragable<%>
    (interface (window<%> area-container<%>)
      after-percentage-change
      set-percentages
      get-percentages
      get-vertical?
      get-default-percentages
      right-click-in-gap
      set-orientation))
  
  (define vertical-dragable<%>
    (interface (dragable<%>)))
  
  (define horizontal-dragable<%>
    (interface (dragable<%>)))
  
  (define dragable-mixin
    (mixin (window<%> area-container<%>) (dragable<%>)
      (init parent)

      (init-field vertical?)
      (define/public-final (get-vertical?) vertical?)
      (define/public-final (set-orientation h?) 
        (define v? (not h?))
        (unless (eq? vertical? v?)
          (set! vertical? v?) 
          (container-flow-modified)))
      (define/private (min-extent child) 
        (let-values ([(w h) (send child get-graphical-min-size)])
          (if (get-vertical?)
              (max (send child min-height) h)
              (max (send child min-width) w))))
      (define/private (event-get-dim evt)
        (if (get-vertical?)
            (send evt get-y)
            (send evt get-x)))
      (define/private (get-gap-cursor)
        (if (get-vertical?)
            (icon:get-up/down-cursor)
            (icon:get-left/right-cursor)))
      
      (define/public (right-click-in-gap evt before after) (void))
      
      (inherit get-client-size container-flow-modified)
      
      (init-field [bar-thickness 5])
      
      ;; percentages : (listof percentage)
      (define percentages null)
      
      ;; get-percentages : -> (listof number)
      (define/public (get-percentages)
        (map percentage-% percentages))
      
      (define/public (set-percentages ps)
        (unless (and (list? ps)
                     (andmap number? ps)
                     (= 1 (apply + ps))
                     (andmap positive? ps))
          (error 'set-percentages 
                 "expected a list of numbers that are all positive and sum to 1, got: ~e"
                 ps))
        (unless (= (length ps) (length (get-children)))
          (error 'set-percentages 
                 "expected a list of numbers whose length is the number of children: ~a, got ~e"
                 (length (get-children))
                 ps))
        (set! percentages (map make-percentage ps))
        (container-flow-modified))
      
      (define/pubment (after-percentage-change) (inner (void) after-percentage-change))
      
      (define/private (get-available-extent)
        (let-values ([(width height) (get-client-size)])
          (- (if (get-vertical?) height width)
             (* bar-thickness (- (length (get-children)) 1)))))
      
      (inherit get-children)
      
      (define/private (update-percentages)
        (let ([len-children (length (get-children))])
          (unless (= len-children (length percentages))
            (cond
              [(zero? len-children)
               (set! percentages '())]
              [else
               (set! percentages (map make-percentage (get-default-percentages len-children)))])
            (after-percentage-change))))
      
      (define/pubment (get-default-percentages i) 
        (define res (inner (if (zero? i) '() (make-list i (/ i)))
                           get-default-percentages i))
        (unless (and (list? res)
                     (andmap (λ (x) (and (real? x) (<= 0 x 1))) res)
                     (= 1 (apply + res))
                     (= (length res) i))
          (error 'get-default-percentages 
                 "expected inner call to return a list of real numbers that sum to 1 and has length ~a"
                 i))
        res)
      
      (define/override (after-new-child child)
        (update-percentages))
      
      (define resizing-dim #f)
      (define resizing-gap #f)
      
      (inherit set-cursor)
      (define/override (on-subwindow-event receiver evt)
        (if (eq? receiver this)
            (let ([gap
                   (ormap (λ (gap) 
                            (and (<= (gap-before-dim gap) 
                                     (event-get-dim evt)
                                     (gap-after-dim gap))
                                 gap))
                          cursor-gaps)])
              (set-cursor (and (or gap
                                   resizing-dim)
                               (let ([c (get-gap-cursor)])
                                 (and (send c ok?)
                                      c))))
              (cond
                [(and gap (send evt button-down? 'right))
                 (right-click-in-gap evt (gap-before gap) (gap-after gap))]
                [(and gap (send evt button-down? 'left))
                 (set! resizing-dim (event-get-dim evt))
                 (set! resizing-gap gap)]
                [(send evt button-up? 'left)
                 (set! resizing-dim #f)
                 (set! resizing-gap #f)]
                [(and resizing-dim resizing-gap (send evt moving?))
                 (let-values ([(width height) (get-client-size)])
                   (let* ([before-percentage (gap-before-percentage resizing-gap)]
                          [orig-before (percentage-% before-percentage)]
                          [after-percentage (gap-after-percentage resizing-gap)]
                          [orig-after (percentage-% after-percentage)]
                          [available-extent (get-available-extent)]
                          [change-in-percentage (/ (- resizing-dim (event-get-dim evt)) available-extent)]
                          [new-before (- (percentage-% before-percentage) change-in-percentage)]
                          [new-after (+ (percentage-% after-percentage) change-in-percentage)])
                     (when ((floor (* new-before available-extent)) . > . (min-extent (gap-before resizing-gap)))
                       (when ((floor (* new-after available-extent)) . > . (min-extent (gap-after resizing-gap)))
                         (set-percentage-%! before-percentage new-before)
                         (set-percentage-%! after-percentage new-after)
                         (after-percentage-change)
                         (set! resizing-dim (event-get-dim evt))
                         (container-flow-modified)))))]
                [else (super on-subwindow-event receiver evt)]))
            (begin
              (set-cursor #f)
              (super on-subwindow-event receiver evt))))
      
      (define cursor-gaps null)
      
      (define/override (place-children _infos width height)
        (update-percentages)
        (define-values (results gaps) 
          (dragable-place-children _infos width height
                                   (map percentage-% percentages)
                                   bar-thickness
                                   (get-vertical?)))        
        (set! cursor-gaps
              (let loop ([children (get-children)]
                         [percentages percentages]
                         [gaps gaps])
                (cond
                  [(null? children) '()]
                  [(null? (cdr children)) '()]
                  [else
                   (define gap (car gaps))
                   (cons (make-gap (car children)
                                   (list-ref gap 0)
                                   (car percentages)
                                   (cadr children)
                                   (list-ref gap 1)
                                   (cadr percentages))
                         (loop (cdr children)
                               (cdr percentages)
                               (cdr gaps)))])))
        
        results)
      
      (define/override (container-size children-info)
        (update-percentages)
        (dragable-container-size children-info bar-thickness (get-vertical?)))
      
      (super-new [parent parent])))

  ;; this function repeatedly checks to see if the current set of percentages and children
  ;; would violate any minimum size constraints. If not, the percentages are used and the
  ;; function termiantes. If some minimum sizes would be violated, the function pulls those 
  ;; children out of the list under consideration, gives them their minimum sizes, rescales
  ;; the remaining percentages back to 1, adjusts the available space after removing those
  ;; panels, and tries again.
  (define (dragable-place-children infos width height percentages bar-thickness vertical?)
    (define original-major-dim-tot (- (if vertical? height width)
                                      (* (max 0 (- (length infos) 1)) bar-thickness)))
    ;; vec : id -o> major-dim size (width)
    (define vec (make-vector (length infos) 0))
    (let loop ([percentages percentages] ;; sums to 1.
               [major-dim-mins (map (λ (info) (if vertical? (list-ref info 1) (list-ref info 0)))
                                    infos)]
               [major-dim-tot original-major-dim-tot]
               [ids (build-list (length percentages) values)])
      (define fitting-ones (extract-fitting-percentages percentages major-dim-mins major-dim-tot))
      (cond
        [(andmap not fitting-ones)
         ;; all of them (perhaps none) fit, terminate.
         (for ([id (in-list ids)]
               [percentage (in-list percentages)])
           (vector-set! vec id (* percentage major-dim-tot)))]
        [else
         ;; something doesn't fit; remove them and try again
         (let ([next-percentages '()]
               [next-major-dim-mins '()]
               [next-major-dim-tot major-dim-tot]
               [next-ids '()])
           (for ([percentage (in-list percentages)]
                 [major-dim-min (in-list major-dim-mins)]
                 [id (in-list ids)]
                 [fitting-one (in-list fitting-ones)])
             (cond
               [fitting-one
                (vector-set! vec id fitting-one)
                (set! next-major-dim-tot (- major-dim-tot fitting-one))]
               [else
                (set! next-percentages (cons percentage next-percentages))
                (set! next-major-dim-mins (cons major-dim-min next-major-dim-mins))
                (set! next-ids (cons id next-ids))]))
           (define next-percentage-sum (apply + next-percentages))
           (loop (map (λ (x) (/ x next-percentage-sum)) next-percentages)
                 next-major-dim-mins
                 next-major-dim-tot
                 next-ids))]))

    ;; adjust the contents of the vector if there are any fractional values
    (let loop ([i 0]
               [maj-val 0])
      (cond
        [(= i (vector-length vec))
         (unless (= maj-val original-major-dim-tot)
           (unless (zero? (vector-length vec))
             (define last-index (- (vector-length vec) 1))
             (vector-set! vec last-index (+ (vector-ref vec last-index) (- original-major-dim-tot maj-val)))))]
        [else
         (vector-set! vec i (floor (vector-ref vec i)))
         (loop (+ i 1)
               (+ maj-val (vector-ref vec i)))]))
    
    ;; build the result for the function from the major dim sizes
    (let loop ([i 0]
               [infos '()]
               [gaps '()]
               [maj-start 0])
      (cond
        [(= i (vector-length vec))
         (values (reverse infos)
                 (reverse gaps))]
        [else
         (define maj-stop (+ maj-start (vector-ref vec i)))
         (define has-gap? (not (= i (- (vector-length vec) 1))))
         (loop (+ i 1)
               (cons (if vertical?
                         (list 0
                               maj-start
                               width
                               (- maj-stop maj-start))
                         (list maj-start
                               0
                               (- maj-stop maj-start)
                               height))
                     infos)
               (if has-gap?
                   (cons (list maj-stop (+ maj-stop bar-thickness)) gaps)
                   gaps)
               (if has-gap?
                   (+ maj-stop bar-thickness)
                   maj-stop))])))
  
  (define (extract-fitting-percentages percentages major-dim-mins major-dim-tot)
    (for/list ([percentage (in-list percentages)]
               [major-dim-min (in-list major-dim-mins)])
      (if (<= major-dim-min (* percentage major-dim-tot))
          #f
          major-dim-min)))
  
  (define (dragable-container-size orig-children-info bar-thickness vertical?)
    (let loop ([children-info orig-children-info]
               [major-size 0]
               [minor-size 0])
      (cond
        [(null? children-info)
         (let ([major-size (+ major-size 
                              (* (max 0 (- (length orig-children-info) 1)) 
                                 bar-thickness))])
           (if vertical?
               (values (ceiling minor-size) (ceiling major-size))
               (values (ceiling major-size) (ceiling minor-size))))]
        [else
         (let ([child-info (car children-info)])
           (let-values ([(child-major major-stretch? child-minor minor-stretch?)
                         (if vertical?
                             ;; 0 = width/horiz, 1 = height/vert
                             (values (list-ref child-info 1)
                                     (list-ref child-info 3)
                                     (list-ref child-info 0)
                                     (list-ref child-info 2))
                             (values (list-ref child-info 0)
                                     (list-ref child-info 2)
                                     (list-ref child-info 1)
                                     (list-ref child-info 3)))])
             (loop (cdr children-info)
                   (+ child-major major-size)
                   (max child-minor minor-size))))])))
  
  (define three-bar-pen-bar-width 8)
  
  (define three-bar-canvas%
    (class canvas%
      (inherit get-dc get-client-size)
      (define/override (on-paint)
        (let ([dc (get-dc)])
          (let-values ([(w h) (get-client-size)])
            (let ([sx (floor (- (/ w 2) (/ three-bar-pen-bar-width 2)))])
              (send dc set-brush (send the-brush-list find-or-create-brush (get-panel-background) 'panel))
              (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
              (send dc draw-rectangle 0 0 w h)
              
              (send dc set-pen (send the-pen-list find-or-create-pen  "black" 1 'solid))
              (send dc draw-line sx 1 (+ sx three-bar-pen-bar-width) 1)
              (send dc draw-line sx 4 (+ sx three-bar-pen-bar-width) 4)
              (send dc draw-line sx 7 (+ sx three-bar-pen-bar-width) 7)
              
              (send dc set-pen (send the-pen-list find-or-create-pen  "gray" 1 'solid))
              (send dc draw-line sx 2 (+ sx three-bar-pen-bar-width) 2)
              (send dc draw-line sx 5 (+ sx three-bar-pen-bar-width) 5)
              (send dc draw-line sx 8 (+ sx three-bar-pen-bar-width) 8)))))
      
      (super-new [style '(no-focus)])
      (inherit stretchable-height min-height)
      (stretchable-height #f)
      (min-height 10)))
  
  (define vertical-dragable-mixin
    (mixin (dragable<%>) (vertical-dragable<%>)
      (super-new [vertical? #t])))
  
  (define horizontal-dragable-mixin
    (mixin (dragable<%>) (horizontal-dragable<%>)
      (super-new [vertical? #f])))
  
  (define vertical-dragable% (vertical-dragable-mixin (dragable-mixin panel%)))
  
  (define horizontal-dragable% (horizontal-dragable-mixin (dragable-mixin panel%)))

  (define splitter<%> (interface () split-horizontal split-vertical collapse))
  ;; we need a private interface so we can use `generic' because `generic'
  ;; doesn't work on mixins
  (define splitter-private<%> (interface () self-vertical? self-horizontal?))

  (define splitter-mixin
   (mixin (area-container<%> dragable<%>) (splitter<%> splitter-private<%>)
    (super-new)
    (inherit get-children add-child
             delete-child
             change-children
             begin-container-sequence
             end-container-sequence)

    (field [horizontal-panel% horizontal-dragable%]
           [vertical-panel% vertical-dragable%])

    (define/public (self-vertical?)
      (send this get-vertical?))
    
    (define/public (self-horizontal?)
      (not (send this get-vertical?)))

    ;; insert an item into a list after some element
    ;; FIXME: this is probably a library function somewhere
    (define/private (insert-after list before item)
      (let loop ([so-far '()]
                 [list list])
        (cond
          [(null? list) (reverse so-far)]
          [(eq? (car list) before) (loop (cons item (cons before so-far))
                                         (cdr list))]
          [else (loop (cons (car list) so-far) (cdr list))])))

    ;; replace an element with a list of stuff
    ;; FIXME: this is probably a library function somewhere
    (define/private (replace list at stuff)
      (let loop ([so-far '()]
                 [list list])
        (cond
          [(null? list) (reverse so-far)]
          [(eq? (car list) at) (append (reverse so-far) stuff (cdr list))]
          [else (loop (cons (car list) so-far) (cdr list))])))

    ;; remove a canvas and merge split panels if necessary
    ;; TODO: restore percentages
    (define/public (collapse canvas)
      (begin-container-sequence)
      (for ([child (get-children)])
        (cond
          [(eq? child canvas)
           (when (> (length (get-children)) 1)
             (change-children
               (lambda (old-children)
                 (remq canvas old-children))))]
          [(is-a? child splitter<%>)
           (send child collapse canvas)]))
      (change-children
        (lambda (old-children)
          (for/list ([child old-children])
            (if (and (is-a? child splitter<%>)
                     (= (length (send child get-children)) 1))
              (let ()
                (define single (car (send child get-children)))
                (send single reparent this)
                single)
              child))))
      (end-container-sequence))

    ;; split a canvas by creating a new editor and either
    ;; 1) adding it to the panel if the panel is already using the same
    ;;   orientation as the split that is about to occur
    ;; 2) create a new panel with the orientation of the split about to
    ;;   occur and add a new editor
    ;;
    ;; in both cases the new editor is returned
    (define/private (do-split canvas maker orientation? orientation% split)
      (define new-canvas #f)
      (for ([child (get-children)])
        (cond
          [(eq? child canvas)
           (begin-container-sequence)
           (change-children
             (lambda (old-children)
               (if (send-generic this orientation?)
                 (let ([new (maker this)])
                   (set! new-canvas new)
                   (insert-after old-children child new))
                 (let ()
                   (define container (new (splitter-mixin orientation%)
                                          [parent this]))
                   (send canvas reparent container)
                   (define created (maker container))
                   (set! new-canvas created)
                   ;; this throws out the old child but we should probably
                   ;; try to keep it
                   (replace old-children child (list container))))))
           (end-container-sequence)]

          [(is-a? child splitter<%>)
           (let ([something (send-generic child split canvas maker)])
             (when something
               (set! new-canvas something)))]))
        new-canvas)

    ;; canvas (widget -> editor) -> editor
    (define/public (split-horizontal canvas maker)
      (do-split canvas maker (generic splitter-private<%> self-horizontal?)
                horizontal-panel% (generic splitter<%> split-horizontal)))

    ;; canvas (widget -> editor) -> editor
    (define/public (split-vertical canvas maker)
      (do-split canvas maker (generic splitter-private<%> self-vertical?)
                vertical-panel% (generic splitter<%> split-vertical)))))

    
  (define discrete-child<%> 
    (interface ()
      get-discrete-widths
      get-discrete-heights))
  
  (define discrete-sizes<%> (interface ((class->interface panel%))
                              get-orientation
                              set-orientation))
  (define (discrete-get-widths c)
    (cond
      [(is-a? c switchable-button%) 
       (if (send c get-label-visible)
           (list (send c get-large-width)
                 (send c get-small-width))
           (list (send c get-without-label-small-width)))]
      [(is-a? c discrete-sizes<%>)
       (send c get-discrete-widths)]
      [else
       #f]))
  
  (define (discrete-get-heights c)
    (cond
      [(is-a? c discrete-sizes<%>)
       (send c get-discrete-heights)]
      [else
       #f]))
  
  (define discrete-sizes-mixin
    (mixin ((class->interface panel%)) (discrete-sizes<%> discrete-child<%>)
      (inherit get-children spacing get-alignment border container-flow-modified
               get-size get-client-size)
      (define horizontal? #t)
      (define/public (get-orientation) horizontal?)
      (define/public (set-orientation h?)
        (unless (equal? horizontal? h?)
          (set! horizontal? h?)
          (container-flow-modified)))
      
      (define/public (get-discrete-widths)
        (cond
          [horizontal?
           (define ws 
             (for/list ([c (in-list (get-children))])
               (discrete-get-widths c)))
           (and (andmap values ws)
                (remove-duplicates
                 (map
                  (λ (x) (apply + x))
                  (candidate-sizes ws))))]
          [else #f]))
      
      (define/public (get-discrete-heights)
        (cond
          [horizontal? #f]
          [else
           (define hs 
             (for/list ([c (in-list (get-children))])
               (discrete-get-heights c)))
           (and (andmap values hs)
                (remove-duplicates
                 (map
                  (λ (x) (apply + x))
                  (candidate-sizes hs))))]))
      
      (define/override (container-size infos)
        (define the-spacing (spacing))
        (define the-border (spacing))
        (define-values (total-min-w total-min-h)
          (for/fold ([w 0] [h 0])
            ([info (in-list infos)]
             [n (in-naturals)])
            (define-values (min-w min-h h-stretch? v-stretch?)
              (apply values info))
            (define this-spacing (if (zero? n) 0 the-spacing))
            (cond
              [horizontal?
               (values (+ w this-spacing min-w)
                       (max h min-h))]
              [else
               (values (max w min-w)
                       (+ h this-spacing min-h))])))
        (define-values (sw sh) (get-size))
        (define-values (cw ch) (get-client-size))
        (values (+ total-min-w the-border the-border
                   (- sw cw))
                (+ total-min-h the-border the-border
                   (- sh ch))))
      
      (define/override (place-children infos w h)
        (define the-spacing (spacing))
        (define the-border (border))
        (define-values (halign valign) (get-alignment))
        (define children (get-children))
        (define all-sizess
          (candidate-sizes
           (for/list ([c (in-list children)]
                      [info (in-list infos)]
                      #:unless (if horizontal?
                                   (and (not (discrete-get-widths c))
                                        (list-ref info 2))
                                   (and (not (discrete-get-heights c))
                                        (list-ref info 3))))
             (if horizontal?
                 (or (discrete-get-widths c)
                     (list (list-ref info 0)))
                 (or (discrete-get-heights c)
                     (list (list-ref info 1)))))))
        (define fitting-sizes
          (for/or ([sizes (in-list all-sizess)])
            (and (<= (apply + sizes) 
                     (- (if horizontal? w h)
                        (* 2 the-border)))
                 sizes)))
        (define fixed-size (apply + fitting-sizes))
        (define number-stretchable
          (for/sum ([info (in-list infos)]
                    [c children])
            (if (if horizontal?
                    (and (not (discrete-get-widths c))
                         (list-ref info 2))
                    (and (not (discrete-get-heights c))
                         (list-ref info 3)))
                1
                0)))
        (define initial-position
          (+ the-border
             (if (zero? number-stretchable)
                 (if horizontal?
                     (case halign
                       [(right) (- w fixed-size)]
                       [(center) (round (/ (- w fixed-size) 2))]
                       [(left) 0])
                     (case valign
                       [(bottom) (- h fixed-size)]
                       [(center) (round (/ (- h fixed-size) 2))]
                       [(top) 0]))
                 0)))
        (define-values (stretchable-size stretchable-leftover)
          (if (zero? number-stretchable)
              (values 0 0)
              (let ([total 
                     (- (if horizontal?
                            w
                            h)
                        fixed-size)])
                (values (quotient total number-stretchable)
                        (modulo total number-stretchable)))))
        (define (take-one) 
          (cond
            [(zero? stretchable-leftover)
             0]
            [else
             (set! stretchable-leftover (- stretchable-leftover 1))
             1]))
        (let loop ([infos infos]
                   [children children]
                   [spot initial-position])
          (cond
            [(null? infos) null]
            [else
             (define-values (min-w min-h h-stretch? v-stretch?)
               (apply values (car infos)))
             (define discrete-child? (if horizontal?
                                         (discrete-get-widths (car children))
                                         (discrete-get-heights (car children))))
             (define this-one
               (cond
                 [(and horizontal? h-stretch? (not discrete-child?))
                  (list spot
                        (round (- (/ h 2) (/ min-h 2)))
                        (+ stretchable-size (take-one))
                        min-h)]
                 [(and (not horizontal?) v-stretch? (not discrete-child?))
                  (list (round (- (/ w 2) (/ min-w 2)))
                        spot
                        min-w
                        (+ stretchable-size (take-one)))]
                 [horizontal?
                  (define size (car fitting-sizes))
                  (set! fitting-sizes (cdr fitting-sizes))
                  (list spot 
                        (round (- (/ h 2) (/ min-h 2)))
                        size
                        min-h)]
                 [else
                  (define size (car fitting-sizes))
                  (set! fitting-sizes (cdr fitting-sizes))
                  (list (round (- (/ w 2) (/ min-w 2)))
                        spot
                        min-w
                        size)]))
             (cons this-one (loop (cdr infos)
                                  (cdr children)
                                  (+ spot
                                     (if horizontal? 
                                         (list-ref this-one 2)
                                         (list-ref this-one 3)))))])))
      
      (super-new)))
  
  (define horizontal-discrete-sizes%
    ;; extra wrapper to get the name right
    (class (discrete-sizes-mixin panel%)
      (super-new)))
  (define vertical-discrete-sizes%
    (class (discrete-sizes-mixin panel%)
      (super-new)
      (inherit set-orientation)
      (set-orientation #f))))


;; candidate-sizes : (listof (listof number)) -> (listof (listof number))
;; in the input, the outer list corresponds to the children for a panel,
;; and each inner list are the sizes that the children can take on.
;; This function returns each possible configuration of sizes, starting
;; with the largest for each and then shrinking each child one size
;; at a time, starting from the earlier children in the list.
;; Note that this will not try all combinations of sizes; once a child
;; has been shrunk one size, larger sizes for that child will not be
;; considered, and shrinking always proceeds from the left to the right.
(define (candidate-sizes lolon)
  (define all-boxes (map (λ (x) (box (sort x >))) lolon))
  (define answer '())
  (define (record-current)
    (set! answer (cons (map car (map unbox all-boxes)) answer)))
  (for ([box (in-list all-boxes)])
    (for ([i (in-range (- (length (unbox box)) 1))])
      (record-current)
      (set-box! box (cdr (unbox box)))))
  (record-current)
  (reverse answer))

(module+ test
  (require rackunit)
  
  (define (log-em lolon) (candidate-sizes lolon))
  
  (check-equal? (log-em '((1)))
                (list '(1)))
  (check-equal? (log-em '((1) (2) (3)))
                (list '(1 2 3)))
  (check-equal? (log-em '((4 3 2 1)))
                (list '(4) '(3) '(2) '(1)))
  (check-equal? (log-em '((1 2 3 4)))
                (list '(4) '(3) '(2) '(1)))
  (check-equal? (log-em '((5 1) (6 2) (7 3)))
                (list '(5 6 7)
                      '(1 6 7)
                      '(1 2 7)
                      '(1 2 3)))
  (check-equal? (log-em '((10 9 8) (7 6 5)))
                (list '(10 7)
                      '(9 7)
                      '(8 7)
                      '(8 6)
                      '(8 5))))
