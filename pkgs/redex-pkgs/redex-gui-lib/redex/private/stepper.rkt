#|

todo:

  - tree diff

  - step until a particular reduction happens (or a choice point is reached)
  
  - break points: supply a function to traces that is a predicate on terms, indicating if
    this one is one where the -> button should stop.

|#

#lang racket/base

(require racket/pretty
         racket/gui/base
         racket/list
         racket/class
         racket/set
         framework
         mrlib/graph
         racket/contract
         "sexp-diffs.rkt"
         "size-snip.rkt"
         redex/private/reduction-semantics)
  
  (provide stepper stepper/seed
           
           ; for testing
           show-diff node%)
  
  (define dot-spacing 20)
  (define dot-size 10)
  (define initial-color "white")
  (define in-path-color "orchid")
  (define visible-color "purple")
  (define cycle-color "yellow")
  (define visible-cycle-color "gold")
  
  (define (pick-label candidate fallback)
    (cond
      [(andmap (λ (x) (send normal-control-font screen-glyph-exists? x #t))
               (string->list candidate))
       candidate]
      [else
       fallback]))
  
  ;; initial-button-label is just used to give some space to the buttons on non-mac platforms
  (define initial-button-label (pick-label "↩→↕" "<->-"))
  
  (define forward-label (pick-label "→" "->"))
  (define updown-label (pick-label "↕" "^"))
  (define back-label (pick-label "↩" "<-"))
  
  (define (stepper red term [pp default-pretty-printer])
    (stepper/seed red (list term) pp))
  
  (define (stepper/seed red seed [pp default-pretty-printer])
    (define term (car seed))
    ;; all-nodes-ht : hash[sexp -o> (is-a/c node%)]
    (define all-nodes-ht (make-hash))
    
    (define root (new node%
                      [pp pp]
                      [all-nodes-ht all-nodes-ht]
                      [term term]
                      [red red]
                      [change-path (λ (new-node) (change-path new-node))]
                      [init-cw (initial-char-width)]))
    
    ;; path : (listof (listof (is-a/c node%))
    ;; the currently visible columns in the pasteboard
    (define path (cons (list root) '()))
    
    (define f (new frame% 
                   [label "PLT Redex Stepper"]
                   [width 700]
                   [height 450]))
    (define dp (new vertical-panel% [parent f]))
    (define upper-hp (new horizontal-panel% [parent dp]))
    (define lower-hp (new horizontal-panel% [alignment '(center center)] [parent f] [stretchable-height #f]))
    (define pb (new columnar-pasteboard% 
                    [moved (λ (a b c d) 
                             (when (procedure? moved)
                               (moved a b c d)))]))
    (define ec (new forward-size-editor-canvas% [parent upper-hp] [editor pb] [style '(#;no-vscroll)]))
    (define bp-outer (new vertical-panel% [parent upper-hp] [stretchable-width #f]))
    (define bp (new vertical-panel% [parent bp-outer] [stretchable-width #f]))
    (define bp-spacer (new grow-box-spacer-pane% [parent bp-outer]))
    
    (define zoom-out-pb (new zoom-out-pasteboard%))
    (define zoom-out-ec (new editor-canvas% 
                             [stretchable-height #t]
                             [parent lower-hp]
                             [style '(hide-vscroll)]
                             [editor zoom-out-pb]))
    
    (define choice-vp (new vertical-panel% [alignment '(center center)] [parent lower-hp] [stretchable-width #f]))
    (define reduction-names (reduction-relation->rule-names red))
    (define reds-choice 
      (and (not (null? reduction-names))
           (new choice%
                [parent choice-vp]
                [font small-control-font]
                [label #f]
                [choices (list* "Single Step"
                                "Step Until Choice"
                                (map (λ (x) (format "Reduce until ~a" x))
                                     reduction-names))])))
    (define red-name-message
      (and (not (null? (reduction-relation->rule-names red)))
           (new message% 
                [parent choice-vp] 
                [stretchable-width #t]
                [font small-control-font] 
                [label ""])))
    (define stupid-internal-definition-syntax1 (new grow-box-spacer-pane% [parent lower-hp]))
    
    (define (update-buttons)
      (let ([last-column (last path)])
        (let ([last-column last-column])
          (let loop ([children (send bp get-children)]
                     [n 0])
            (cond
              [(= n (length last-column)) 
               (send bp change-children
                     (λ (l)
                       (filter (λ (p) (not (memq p children))) l)))
               (void)]
              [(null? children)
               (new button-object% 
                    [parent bp]
                    [n n])
               (loop children
                     (+ n 1))]
              [else
               (loop (cdr children)
                     (+ n 1))])))
        (let ([button-objects (send bp get-children)])
          (if (null? (cdr button-objects))
              (send (car button-objects) hide-vertical)
              (for-each (λ (x) (send x show-vertical))
                        button-objects))
          (for-each (λ (node button-object)
                      (cond
                        [(not (null? (send node get-cycle)))
                         (send button-object step-goes-back #t)
                         (send button-object enable-step #f)]
                        [else
                         (send button-object step-goes-back #f)
                         (send button-object enable-step (not (null? (send node get-successors))))]))
                    last-column
                    button-objects))))
    
    (define button-object%
      (class vertical-panel%
        (init-field n)
        (super-new [style '(border)]
                   [alignment '(left center)])
        
        (inherit change-children)
        (define/public (hide-vertical)
          (change-children (λ (x) (remq expand-button x))))
        (define/public (show-vertical)
          (change-children (λ (x) (if (memq expand-button x)
                                      x
                                      (append x (list expand-button))))))
        
        (define/public (enable-step on?)
          (send step-button enable on?))
        
        (define/public (step-goes-back back?)
          (send step-button set-label 
                (if back? 
                    back-label 
                    forward-label)))
        
        (define step-button
          (new button%
               [label initial-button-label]
               [callback (λ (x y) (forward-step n))]
               [parent this]))
        (define expand-button
          (new button% 
               [label initial-button-label] 
               [callback (λ (x y) (expand n))] 
               [parent this]))
        
        (send step-button set-label forward-label)
        (send expand-button set-label updown-label)))
    
    (define (forward-step n)
      (let* ([last-pr (last-pair path)]
             [last-column (car last-pr)]
             [click-target (list-ref last-column n)])
        (cond
          [(not (null? (send click-target get-cycle)))
           (void)]
          [else
           (let ([new-path-tail (iterate-until-done click-target)])
             (for-each (λ (x) (send x set-in-path? (eq? x click-target))) last-column)
             (for-each (λ (new-children)
                         (for-each (λ (x) (send x set-in-path? #t)) new-children))
                       new-path-tail)
             (set! path
                   (let loop ([path path])
                     (cond
                       [(null? (cdr path))
                        (cons (list click-target)
                              new-path-tail)]
                       [else (cons (car path) (loop (cdr path)))])))
             (update-everything)
             (update-highlight-to-end))])))
    
    ;; iterate-until-done : node -> (listof (listof node))
    ;; iterates forward in the path until a choice point is reached
    ;; or until we hit a stopping point. return the new tail of the path
    (define (iterate-until-done click-target)
      (let ([looking-for
             (cond
               [(or (not reds-choice)
                    (zero? (send reds-choice get-selection)))
                #f]
               [(equal? (send reds-choice get-selection) 1)
                #t]
               [else (symbol->string
                      (list-ref reduction-names (- (send reds-choice get-selection) 2)))])])
        (let loop ([next-node click-target]
                   [new-nodes (list)]
                   [cutoff (if looking-for
                               100
                               1)])
          (cond
            [(zero? cutoff) (reverse new-nodes)]
            [else
             (let ([new-children (begin (send next-node force)
                                        (send next-node get-children))])
               (cond
                 [(null? new-children)
                  (reverse new-nodes)]
                 [(null? (cdr new-children))
                  (cond
                    [(send (car new-children) in-cycle?)
                     (reverse (cons new-children new-nodes))]
                    [(and (not (eq? looking-for #t))
                          (member looking-for (find-reduction-label next-node (car new-children) #f)))
                     (reverse (cons new-children new-nodes))]
                    [else
                     (loop (car new-children)
                           (cons new-children new-nodes)
                           (- cutoff 1))])]
                 [else
                  (reverse (cons new-children new-nodes))]))]))))
    
    (define (expand n)
      (let* ([last-pr (last-pair path)]
             [last-column (car last-pr)]
             [survivor (list-ref last-column n)])
        (for-each (λ (x) (send x set-in-path? (eq? x survivor))) last-column)
        (set! path
              (let loop ([path path])
                (cond
                  [(null? (cdr path)) 
                   (cons (list survivor) '())]
                  [else
                   (cons (car path) (loop (cdr path)))])))
        (update-everything)
        (update-highlight-to-end)))
    
    (define (moved left top right bottom)
      (let ([bx (box 0)])
        (let loop ([path path])
          (cond
            [(null? path) (void)]
            [else
             (let* ([path-ele (car path)]
                    [snip (send (car path-ele) get-big-snip)]
                    [visible?
                     (or (begin (send pb get-snip-location snip bx #f #f)
                                (<= left (unbox bx) right))
                         (begin (send pb get-snip-location snip bx #f #t)
                                (<= left (unbox bx) right)))])
               (for-each (λ (node) (send node set-visible? visible?))
                         path-ele))
             (loop (cdr path))]))))
    
    (define (get-path-to-root node)
      (let loop ([node node]
                 [acc null])
        (let ([parents (send node get-parents)])
          (cond
            [(null? parents) (cons (list node) acc)]
            [node (loop (car parents) 
                        (cons (list node) acc))]))))
    
    (define (change-path new-node)
      (cond
        [(ormap (λ (l) (memq new-node l)) path)
         ;; if this node is in the current path, just move the view
         (let* ([snip (send new-node get-big-snip)]
                [br (box 0)])
           (send pb get-snip-location snip br #f #t)
           (let ([bw (box 0)]
                 [bh (box 0)])
             (send (send (send ec get-editor) get-admin) get-view #f #f bw bh)
             (let* ([x (max 0 (- (unbox br) (unbox bw)))])
               (send ec scroll-to x 0.0 (- (unbox bw) 4) (- (unbox bh) 1) #t 'end))))]
        [else
         (let ([new-path (get-path-to-root new-node)])
           (let loop ([new-path new-path]
                      [path path])
             (cond
               [(or (null? path)
                    (null? new-path)
                    (not (equal? (car path) (car new-path))))
                (for-each (λ (old-ele) (for-each (λ (x) (send x set-in-path? #f)) old-ele))
                          path)
                (for-each (λ (old-ele) (for-each (λ (x) (send x set-in-path? #t)) old-ele))
                          new-path)]
               [else
                (loop (cdr new-path) (cdr path))]))
           
           (set! path new-path)
           (update-everything))])
      (update-highlight-to-node-and-parent new-node))
    
    (define (update-everything)
      (send pb begin-edit-sequence)
      (pb-change-columns)
      (pb-last-column-visible)
      (send pb end-edit-sequence)
      (update-buttons))
    
    (define (update-highlight-to-end)
      (let-values ([(one-col-before last-column)
                    (let loop ([path path]
                               [one-before #f]
                               [last-one #f])
                      (cond
                        [(null? path) (values one-before last-one)]
                        [else (loop (cdr path)
                                    last-one
                                    (car path))]))])
        (when (and one-col-before
                   last-column
                   (= 1 (length one-col-before))
                   (= 1 (length last-column)))
          (set-highlight (car one-col-before)
                         (car last-column)))))
    
    (define (update-highlight-to-node-and-parent node)
      (let* ([all-parents (send node get-parents)]
             [visible-parent
              (ormap (λ (x) (and (memq x all-parents) x))
                     (apply append path))])
        (when visible-parent
          (set-highlight visible-parent node))))
    
    (define (set-highlight parent child)
      (for-each
       (λ (col)
         (for-each (λ (node) (send (send node get-big-snip) clear-diffs))
                   col))
       path)
      
      (show-diff parent child)
      
      (when red-name-message
        (let ([label (map (λ (x) (if x (format "[~a]" x) "≪unknown≫"))
                          (find-reduction-label parent child #t))])
          (cond
            [(null? label) (void)]
            [(null? (cdr label))
             (send red-name-message set-label (car label))]
            [else
             (apply 
              string-append
              (car label)
              (map (λ (x) (format " and ~a" x))
                   (cdr label)))]))))
    
    (define (find-reduction-label parent child computed?)
      (let ([children (send parent get-children)])
        (and children
             (let loop ([children children]
                        [red-names (if computed?
                                       (send parent get-successor-computed-names)
                                       (send parent get-successor-names))])
               (cond
                 [(null? children) #f]
                 [else
                  (if (eq? (car children) child)
                      (car red-names)
                      (loop (cdr children)
                            (cdr red-names)))])))))
    
    (define (pb-change-columns)
      (send pb change-columns (map (λ (l) (map (λ (x) (send x get-big-snip)) l)) 
                                   path))
      (send zoom-out-pb refresh-tree root))
    
    ;; makes the last column visible
    (define (pb-last-column-visible)
      (let ([admin (send pb get-admin)]
            [sl (box 0)]
            [st (box 0)]
            [sr (box 0)]
            [sb (box 0)])
        (when admin
          ;; reverse so the topmost snip is the last one
          (for ([node (in-list (reverse (car (last-pair path))))])
            (let ([s (send node get-big-snip)])
              (send pb get-snip-location s sl st #f)
              (send pb get-snip-location s sr sb #t)
              (send pb scroll-to s 0 0 (- (unbox sr) (unbox sl)) (- (unbox sb) (unbox st)) #t))))))
    
    (hash-set! all-nodes-ht term root)
    (send root set-in-path? #t)
    
    (let loop ([term (car seed)]
               [last-nexts #f]
               [terms (cdr seed)])
      
      (when last-nexts
        (expand (find-i term last-nexts void)))
      (cond
        [(null? terms) (void)]
        [else
         (let* ([nexts (apply-reduction-relation red term)]
                [ith (find-i (car terms)
                             nexts
                             (λ ()
                               (error 'stepper "term ~s does not reduce to ~s"
                                      term 
                                      (car terms))))])
           (forward-step 0)
           (loop (car terms)
                 nexts
                 (cdr terms)))]))
    
    (send f show #t)
    (pb-change-columns)
    (update-buttons))
  
  (define (show-diff parent child)
    (let-values ([(to-color1 to-color2) 
                  (find-differences 
                   (send parent get-term)
                   (send child get-term)
                   (send (send parent get-big-snip) get-char-width)
                   (send (send child get-big-snip) get-char-width))])
      (send (send parent get-big-snip) highlight-diffs to-color1)
      (send (send child get-big-snip) highlight-diffs to-color2)
      (void)))
  
  (define (find-i term terms fail)
    (let loop ([i 0]
               [terms terms])
      (cond
        [(null? terms) (fail)]
        [(equal? (car terms) term) i]
        [else (loop (+ i 1) (cdr terms))])))
  
  (define node%
    (class object%
      (init-field term 
                  red
                  change-path
                  all-nodes-ht
                  pp
                  init-cw)
      (init [parent #f])
      
      (define parents (if parent 
                          (list parent)
                          '()))
      ;; cycle : (listof node) 
      ;; the nodes that have the same term as this one, due to a cycle in the reduction graph
      (define cycle '())
      (define children #f)
      (define big-snip (mk-big-snip term this pp init-cw))
      (define dot-snip (new dot-snip% [node this]))
      (define in-path? #f)
      (define visible? #f)
    
      (define successors #f)

      ;; #f => uninited, else
      ;; (listof (listof string))
      ;; one list element for each successor, one nested list element for each reduction that applied (typically 1)
      (define successor-names #f)
      (define successor-computed-names #f)
      (define/public (get-successors)
        (unless successors
          (let-values ([(succs names comp-names)
                        (for/fold ([succs (set)]
                                   [names #hash()]
                                   [comp-names #hash()])
                          ([reduction (apply-reduction-relation/tagged red term)])
                          (let ([name (first reduction)]
                                [comp-name (second reduction)]
                                [succ (third reduction)]
                                [add (λ (x) (λ (xs) (cons x xs)))])
                            (values (set-add succs succ)
                                    (hash-update names succ (add name) '())
                                    (hash-update comp-names succ (add comp-name) '()))))])
            (set! successors (set-map succs values))
            (set! successor-names (map (λ (s) (hash-ref names s)) successors))
            (set! successor-computed-names (map (λ (s) (hash-ref comp-names s)) successors))))
        successors)
      (define/public (get-successor-names)
        (get-successors) ;; force the variables to be defined
        successor-names)
      (define/public (get-successor-computed-names)
        (get-successors) ;; force the variables to be defined
        successor-computed-names)
      
      (define/public (move-path)
        (change-path this))
      
      (define/public (set-in-path? p?)
        (set! in-path? p?)
        (update-color))
      
      (define/public (set-visible? v?)
        (set! visible? v?)
        (update-color))
      
      (define/private (update-color)
        (send dot-snip set-color 
              (cond
                [(and visible? in-path? (not (null? cycle)))
                 visible-cycle-color]
                [(not (null? cycle))
                 cycle-color]
                [(and visible? in-path?)
                 visible-color]
                [in-path?
                 in-path-color]
                [else
                 initial-color])))
      
      (define/public (get-cycle) cycle)
      (define/public (add-cycle c) (set! cycle (cons c (remq c cycle))))
      (define/public (in-cycle?) (not (null? cycle)))
      (define/public (get-term) term)
      (define/public (get-big-snip) big-snip)
      (define/public (get-dot-snip) dot-snip)
      (define/public (get-parents) parents)
      (define/public (add-parent p)
        (add-links (send p get-dot-snip) dot-snip)
        (set! parents (cons p parents)))
      (define/public (get-children) (or children '()))
      (define/public (force)
        (unless children
          (set! children
                (map (λ (x) (make-child x)) (get-successors)))))
      
      (define/private (make-child term)
        (let ([already-there (hash-ref all-nodes-ht term #f)]
              [mk-child-node
               (λ ()
                 (new node%
                      [pp pp]
                      [term term]
                      [red red]
                      [change-path change-path]
                      [all-nodes-ht all-nodes-ht]
                      [parent this]
                      [init-cw init-cw]))])
          (cond
            [(and already-there
                  (or (eq? this already-there)
                      (is-parent? already-there)))
             (let ([n (mk-child-node)])
               (send n add-cycle already-there)
               (send already-there add-cycle n)
               n)]
            [already-there
             (send already-there add-parent this)
             already-there]
            [else
             (let ([child-node (mk-child-node)])
               (hash-set! all-nodes-ht term child-node)
               child-node)])))
      
        (define/private (is-parent? node)
          (let loop ([parents (get-parents)])
            (ormap (λ (p)
                     (or (eq? p node)
                         (loop (send p get-parents))))
                   parents)))
        
      (super-new)
      (when cycle
        (send dot-snip set-color cycle-color))
      (when parent
        (add-links (send parent get-dot-snip) dot-snip))))
    
  (define zoom-out-pasteboard%
    (class (graph-pasteboard-mixin pasteboard%)
      (inherit insert move-to get-canvas get-admin)

      (inherit find-snip set-caret-owner global-to-local)
      (define/override (on-event evt)
        (when (send evt button-down?)
          (let ([x (box (send evt get-x))]
                [y (box (send evt get-y))])
            (global-to-local x y)
            (let ([s (find-snip (unbox x) (unbox y))])
              (when s
                (set-caret-owner s 'immediate)))))
        (super on-event evt))
      
      (define/public (refresh-tree root)
        (let ([level-ht (make-hasheq)]
              [node-to-level-ht (make-hasheq)]
              [max-n 0])
          
          (let loop ([tree root]
                     [n 0])
            (let ([old-level (hash-ref node-to-level-ht tree #f)])
              (cond
                [(not old-level)
                 (hash-set! node-to-level-ht tree n)
                 (hash-set! level-ht n (cons tree (hash-ref level-ht n '())))]
                [(< old-level n)
                 (hash-set! level-ht old-level (remq tree (hash-ref level-ht old-level)))
                 (hash-set! level-ht n (cons tree (hash-ref level-ht n '())))
                 (hash-set! node-to-level-ht tree n)]
                [else
                 (void)])
              (set! max-n (max n max-n))
              (for-each (λ (x) (loop x (+ n 1))) (send tree get-children))))
          
          (let* ([tallest-column (apply max (hash-map level-ht (λ (x y) (length y))))]
                 [canvas (get-canvas)]
                 [_1 (send canvas min-client-height (* tallest-column dot-spacing))]
                 [vertical-space
                  (let-values ([(w h) (send canvas get-client-size)]) h)])
            (let loop ([n 0])
              (when (<= n max-n)
                (let ([nodes (reverse (hash-ref level-ht n))])
                  (let loop ([nodes nodes]
                             [y (/ (- vertical-space
                                      (* (length nodes) dot-spacing)) 
                                   2)])
                    (cond
                      [(null? nodes) (void)]
                      [else
                       (let* ([node (car nodes)]
                              [dot-snip (send node get-dot-snip)])
                         (insert dot-snip (* n dot-spacing) y)   ;; in case the snip's been inserted already
                         (move-to dot-snip (* n dot-spacing) y)  ;; also do the move to
                         (loop (cdr nodes) (+ y dot-spacing)))])))
                (loop (+ n 1)))))))
      (super-new)
      (send this set-flip-labels? #f)
      (inherit set-draw-arrow-heads?)
      (set-draw-arrow-heads? #f)))

  (define (set-box/f b v) (when (box? b) (set-box! b v)))
  
  (define dot-snip%
    (class (graph-snip-mixin snip%)
      (init-field node)
      
      (inherit get-admin)
      (define color initial-color)
      (define/public (set-color c)
        (unless (equal? color c)
          (set! color c)
          (let ([admin (get-admin)])
            (when admin
              (send admin needs-update this 0 0 dot-size dot-size)))))
      (define/override (get-extent dc x y wb hb descentb spaceb lspaceb rspaceb)
        (set-box/f wb dot-size)
        (set-box/f hb dot-size)
        (set-box/f descentb 0)
        (set-box/f spaceb 0)
        (set-box/f lspaceb 0)
        (set-box/f rspaceb 0))
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([smoothing (send dc get-smoothing)]
              [brush (send dc get-brush)])
          (send dc set-smoothing 'aligned)
          (send dc set-brush color 'solid)
          (send dc draw-ellipse x y dot-size dot-size)
          (send dc set-brush brush)
          (send dc set-smoothing smoothing)))
      (define/override (on-event dc x y editorx editory evt)
        (when (send evt button-up?)
          (send node move-path)))
      
      (define/override (copy) (new snip%))
      (super-new)
      (inherit set-snipclass set-flags get-flags)
      (set-flags (cons 'handles-events (get-flags)))
      (set-snipclass dot-snipclass)))
  
  (define dot-snipclass
    (new
     (class snip-class%
       (define/override (read f)
         (new dot-snip%))
       (super-new))))
  (send dot-snipclass set-classname "plt-redex:dot")
  (send dot-snipclass set-version 1)
  (send (get-the-snip-class-list) add dot-snipclass)
       
  
  (define forward-size-editor-canvas%
    (class canvas:basic% 
      (inherit get-editor)
      (define/override (on-size w h)
        (send (get-editor) update-heights))
      (super-new)))
  
  (define (mk-big-snip sexp node pp init-cw)
    (let* ([txt (new text:keymap%)]
           [s (new big-snip% 
                   [pp pp]
                   [node node]
                   [editor txt]
                   [expr sexp]
                   [char-width (get-user-char-width init-cw sexp)])])
      (send txt set-autowrap-bitmap #f)
      #;(send txt freeze-colorer)
      (send s format-expr)
      s))
  
  (define big-snip%
    (class size-editor-snip%
      (inherit get-editor)
      (init-field node)
      (define/public (get-node) node)
      (define clear-thunks '())
      (define/augment (on-width-changed w)
        (clear-diffs)
        (inner (void) on-width-changed w))
      (define/public (highlight-diffs to-color)
        (clear-diffs)
        (set! clear-thunks
              (map 
               (λ (p) (send (get-editor) highlight-range
                            (car p)
                            (cdr p)
                            (send the-color-database find-color "NavajoWhite")))
               to-color)))
      (define/public (clear-diffs)
        (for-each (λ (t) (t)) clear-thunks)
        (set! clear-thunks null))
      (super-new)))
  
  (define columnar-pasteboard% 
    (class (resizing-pasteboard-mixin pasteboard%)
      (init-field moved)
      
      (define current-columns '())
      (inherit insert remove find-snip)
      
      ;; strange to think that this is the way to catch 
      ;; different snips becoming visible in the editor, but oh well.
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (unless before?
          (let ([admin (get-admin)])
            (when admin
              (let ([bx (box 0)]
                    [by (box 0)]
                    [bw (box 0)]
                    [bh (box 0)])
                (send admin get-view bx by bw bh)
                (moved (unbox bx)
                       (unbox by)
                       (+ (unbox bx) (unbox bw))
                       (+ (unbox by) (unbox bh))))))))
      
      (define/public (change-columns orig-new-columns)
        (let loop ([current-columns current-columns]
                   [new-columns orig-new-columns])
          (cond
            [(and (null? current-columns)
                  (null? new-columns))
             (void)]
            [(null? new-columns) 
             (insert/remove current-columns '())]
            [(null? current-columns)
             (insert/remove '() new-columns)]
            [(equal? (car current-columns)
                     (car new-columns))
             (loop (cdr current-columns)
                   (cdr new-columns))]
            [else
             (insert/remove current-columns new-columns)]))
        (set! current-columns orig-new-columns)
        (update-heights))
      
      ;; insert/remove : (listof (listof snip)) (listof (listof snip)) -> void
      (define/private (insert/remove to-remove to-insert)
        (let ([flat-to-remove (apply append to-remove)]
              [flat-to-insert (apply append to-insert)])
          (for-each
           (λ (x) 
             (unless (memq x flat-to-insert)
               (remove x)))
           flat-to-remove)
          (for-each (λ (x) (insert x)) flat-to-insert)))
      
      (inherit get-admin move-to)
      (define/public (update-heights)
        (let ([admin (get-admin)])
          (let-values ([(w h) (get-view-size)])
            (let loop ([columns current-columns]
                       [x 0])
              (cond
                [(null? columns) (void)]
                [else
                 (let* ([column (car columns)])
                   (cond
                     [(null? (cdr column))
                      ;; if there is only a single snip in the column, we let it be as long as it wants to be.
                      (let* ([snip (car column)]
                             [sw (get-snip-width snip)]
                             [sh (get-snip-max-height snip)]
                             [new-height (- (max h sh) (get-border-height snip))])
                        (move-to snip x 0)
                        (send snip set-min-height new-height)
                        (send snip set-max-height new-height)
                        (loop (cdr columns) (+ x sw)))]
                     [else
                      ;; otherwise, we make all of the snips fit into the visible area
                      (let* ([base-space (quotient h (length column))]
                             [widest
                              (let loop ([snips column]
                                         [extra-space (modulo h (length column))]
                                         [y 0]
                                         [widest 0])
                                (cond
                                  [(null? snips) widest]
                                  [else 
                                   (let* ([snip (car snips)]
                                          [sw (get-snip-width snip)]
                                          [h (+ base-space
                                                (if (zero? extra-space)
                                                    0
                                                    1))])
                                     (move-to snip x y)
                                     (let ([border-height (get-border-height snip)])
                                       (send snip set-min-height (- h border-height))
                                       (send snip set-max-height (- h border-height)))
                                     (loop (cdr snips)
                                           (if (zero? extra-space)
                                               0
                                               (- extra-space 1))
                                           (+ y h)
                                           (max widest sw)))]))])
                          (for-each (λ (snip) 
                                      (let ([border-width (get-border-width snip)])
                                        (send snip set-min-width (- widest border-width))
                                        (send snip set-max-width (- widest border-width))))
                                    column)
                        (loop (cdr columns)
                              (+ x widest)))]))])))))

      (define/private (get-border-height snip)
        (let ([lb (box 0)]
              [tb (box 0)]
              [rb (box 0)]
              [bb (box 0)])
          (send snip get-margin lb tb bb rb)
          (+ (unbox bb) (unbox tb))))
      
      (define/private (get-border-width snip)
        (let ([lb (box 0)]
              [tb (box 0)]
              [rb (box 0)]
              [bb (box 0)])
          (send snip get-margin lb tb bb rb)
          (+ (unbox lb) (unbox rb))))
      
      (inherit get-snip-location)
      (define/public (get-snip-width snip)
        (let ([lb (box 0)]
              [rb (box 0)])
          (get-snip-location snip lb #f #f)
          (get-snip-location snip rb #f #t)
          (- (unbox rb) (unbox lb))))
      
      ;; get-snip-max-height : snip -> number
      ;; returns the maximum height that the snip wants to be 
      ;; (ie, the end position of the longest line)
      (define/private (get-snip-max-height snip)
        (let ([txt (send snip get-editor)]
              [yb (box 0)]
              [tb (box 0)]
              [bb (box 0)])
          (send snip get-margin (box 0) tb (box 0) bb)
          (send txt position-location (send txt last-position) #f yb #f #t #t)
          (+ (unbox yb)
             (unbox tb)
             (unbox bb))))
          
      (define/private (get-view-size)
        (let ([admin (get-admin)])
          (if admin
              (let ([wb (box 0)]
                    [hb (box 0)])
                (send admin get-view #f #f wb hb)
                (values (unbox wb) (- (unbox hb) 2)))
              (values 10 10))))
      
      (super-new)))
