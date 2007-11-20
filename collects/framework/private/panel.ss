
#lang scheme/unit
  (require (lib "class.ss")
           "sig.ss"
           (lib "mred-sig.ss" "mred")
           (lib "list.ss")
           (lib "etc.ss"))
  
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
                                   "alignment spec is unknown ~a~n" spec)])))])
            (map (λ (l) 
                   (let*-values ([(min-width min-height v-stretch? h-stretch?)
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
      get-vertical?))
  
  (define vertical-dragable<%>
    (interface (dragable<%>)))
  
  (define horizontal-dragable<%>
    (interface (dragable<%>)))
  
  (define dragable-mixin
    (mixin (window<%> area-container<%>) (dragable<%>)
      (init parent)
      
      (define/public (get-vertical?)
        (error 'get-vertical "abstract method"))
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
            (let ([rat (/ 1 len-children)])
              (set! percentages (build-list len-children (λ (i) (make-percentage rat)))))
            (after-percentage-change))))
      
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
        (set! cursor-gaps null)
        (update-percentages)
        (cond
          [(null? _infos) null]
          [(null? (cdr _infos)) (list (list 0 0 width height))]
          [else
           (let ([available-extent (get-available-extent)]
                 [show-error
                  (λ (n)
                    (error 'panel.ss::dragable-panel "internal error.~a" n))])
             (let loop ([percentages percentages]
                        [children (get-children)]
                        [infos _infos]
                        [dim 0])
               (cond
                 [(null? percentages)
                  (unless (null? infos) (show-error 1))
                  (unless (null? children) (show-error 2))
                  null]
                 [(null? (cdr percentages))
                  (when (null? infos) (show-error 3))
                  (when (null? children) (show-error 4))
                  (unless (null? (cdr infos)) (show-error 5))
                  (unless (null? (cdr children)) (show-error 6))
                  (if (get-vertical?)
                      (list (list 0 dim width (- height dim)))
                      (list (list dim 0 (- width dim) height)))]
                 [else
                  (when (null? infos) (show-error 7))
                  (when (null? children) (show-error 8))
                  (when (null? (cdr infos)) (show-error 9))
                  (when (null? (cdr children)) (show-error 10))
                  (let* ([info (car infos)]
                         [percentage (car percentages)]
                         [this-space (floor (* (percentage-% percentage) available-extent))])
                    (set! cursor-gaps (cons (make-gap (car children)
                                                      (+ dim this-space)
                                                      percentage
                                                      (cadr children)
                                                      (+ dim this-space bar-thickness)
                                                      (cadr percentages))
                                            cursor-gaps))
                    (cons (if (get-vertical?)
                              (list 0 dim width this-space)
                              (list dim 0 this-space height))
                          (loop (cdr percentages)
                                (cdr children)
                                (cdr infos)
                                (+ dim this-space bar-thickness))))])))]))
      
      (define/override (container-size children-info)
        (update-percentages)
        (let loop ([percentages percentages]
                   [children-info children-info]
                   [major-size 0]
                   [minor-size 0])
          (cond
            [(null? children-info)
             (if (get-vertical?)
                 (values (ceiling minor-size) (ceiling major-size))
                 (values (ceiling major-size) (ceiling minor-size)))]
            [(null? percentages)
             (error 'panel.ss::dragable-panel "internal error.12")]
            [else
             (let ([child-info (car children-info)]
                   [percentage (car percentages)])
               (let-values ([(child-major major-stretch? child-minor minor-stretch?)
                             (if (get-vertical?)
                                 (values (list-ref child-info 1)
                                         (list-ref child-info 3)
                                         (list-ref child-info 0)
                                         (list-ref child-info 2))
                                 (values (list-ref child-info 0)
                                         (list-ref child-info 2)
                                         (list-ref child-info 1)
                                         (list-ref child-info 3)))])
                 (loop (cdr percentages)
                       (cdr children-info)
                       (max (/ child-major (percentage-% percentage)) major-size)
                       (max child-minor minor-size))))])))
      
      (super-instantiate (parent))))
  
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
      
      (super-instantiate ())
      (inherit stretchable-height min-height)
      (stretchable-height #f)
      (min-height 10)))
  
  
  (define vertical-dragable-mixin
    (mixin (dragable<%>) (vertical-dragable<%>)
      (define/override (get-vertical?) #t)
      (super-instantiate ())))
  
  (define horizontal-dragable-mixin
    (mixin (dragable<%>) (vertical-dragable<%>)
      (define/override (get-vertical?) #f)
      (super-instantiate ())))
  
  (define vertical-dragable% (vertical-dragable-mixin (dragable-mixin vertical-panel%)))
  
  (define horizontal-dragable% (horizontal-dragable-mixin (dragable-mixin horizontal-panel%)))

