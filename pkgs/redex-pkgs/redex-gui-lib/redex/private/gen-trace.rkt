#lang racket/base

(require racket/contract
         racket/set
         racket/function
         racket/match
         racket/vector
         racket/list
         racket/class
         racket/pretty
         racket/gui/base
         racket/string
         redex/private/jdg-gen
         redex/private/judgment-form
         redex/private/search
         redex/private/trace-layout
         (only-in redex/private/pat-unify
                  dq
                  env-eqs
                  env-dqs))

(provide make-tree
         show-trace
         raw-locs
         format-trace)

;; tree browser prototype
;; 
;; uses a mapping to a geometric series to get an "axis at infinity" effect
;;
;; TODO:
;; * the layout algorithm is pretty dumb right now, could definitely be improved
;; * zooming/panning starts getting slow for trees with around 1,000 nodes
;;


(define loc? (listof exact-nonnegative-integer?))
(define trace-element? (cons/c loc? list?))
(define trace? (listof trace-element?))
;; well-formed trace: nodes with no siblings have a parent
;; and nodes that aren't the oldest sibling (index 0) have a next-youngest
;; this is a little slow right now - mabe overkill, but probably will have a similar
;; complexity to a layout algorithm that scans the entire trace
(define (trace-is-well-formed? trace)
  (define locs (set))
  (define (add-check-for-replaces loc)
    (set! locs (set-remove-all locs (curry loc-prefix-same? loc)))
    (set! locs (set-add locs loc)))
  (for/and ([t-e (in-list trace)])
    (add-check-for-replaces (car t-e))
    (match t-e
      [`(() ,rest ...)
       #t]
      [`((,locs-parent ... 0) ,rest ...)
       (set-member? locs locs-parent)]
      [`((,locs-parent ... ,n) ,rest ...)
       (set-member? locs `(,@locs-parent ,(- n 1)))])))
(define well-formed-trace? (and/c trace? trace-is-well-formed?))
         
(define (show-trace)
  (define tr (get-most-recent-trace))
  (when tr
    (define trace (format-trace tr))
    (when trace
      (show-trace-frame trace))))

;(define-struct gen-trace (tr-loc clause input state bound env) #:prefab)
#;
(vector 'info 
        "generation-log: yes" 
        (gen-trace '() (clause '(list (list (list))) '() '()'yes) '(list (list (name l_0 (nt l)))) #t 5 (env '#hash() '())) 
        'generation-log)

(define (format-trace tr)
  (map (match-lambda
             [(vector 'info clause-name
                      (gen-trace tr-loc clause input state bound env) 'generation-log)
              (list (reverse tr-loc) clause-name state input (clause-head-pat clause) bound 0 env)])
           tr))

(define/contract (show-trace-frame trace)
  (-> well-formed-trace? any)
  (define tf (new tree-frame% [t trace] [w 600] [h 600]))
  (send tf display))

(define (set-remove-all old-set pred?)
  (for/fold ([new-s (set)])
    ([s old-set])
    (if (pred? s)
        new-s
        (set-add new-s s))))

(define (loc-prefix-same? parent child)
  (and (<= (length parent) (length child))
       (for/and ([e1 parent] [e2 child])
         (equal? e1 e2))))

(struct gen-tree
  (loc attributes [children #:mutable])
  #:transparent)

(struct attributes
  (label id in-bound term body coords [focus #:mutable] env)
  #:transparent)

(struct coords
  (x y)
  #:transparent
  #:mutable)

(struct trace-step (path b-factor attributes)
  #:transparent)

(struct lvar (id) #:prefab)

(define SCROLL-RANGE 10000)

;; doesn't currently use all the information in the trace
(define (make-tree trace)
  (let loop ([tree (vector 0)]
             [t trace])
    (match t
      [`((,loc ,name ,state ,term ,body ,bound ,depth ,env) ,remaining-traces ...)
       (define atts (attributes name (gensym) (positive? bound) term body (coords #f #f) #f env))
       (loop (insert-tree-atts loc atts tree) remaining-traces)]
      [_
       tree])))

(define (trace-step-loc t-step)
  (match t-step
      [`(,loc ,name ,state ,term ,body ,bound ,depth ,env)
       loc]))

(define (trace-step->atts t-step)
  (match t-step
      [`(,loc ,name ,state ,term ,body ,bound ,depth ,env)
       (attributes name (gensym) (positive? bound) term body (coords #f #f) #f env)]))

(define (insert-tree-atts loc attributes tree-root)
  (insert-tree-node loc (gen-tree loc attributes (make-vector 0)) tree-root))

(define (insert-tree-node full-loc node tree-root)
  (define (insert-node loc node tree)
    (match loc
      [`(,i) ;; append or replace
       (cond [(or (= 0 (vector-length (gen-tree-children tree)))
                  (not (vector-member full-loc
                                 (vector-map gen-tree-loc (gen-tree-children tree)))))
              (define new-children (vector-append (gen-tree-children tree)
                                                  (vector node)))
              (set-gen-tree-children! tree new-children)
              #f]
             [else
              (define old-node 
                (vector-ref (gen-tree-children tree) 
                            (vector-member full-loc
                                           (vector-map gen-tree-loc (gen-tree-children tree)))))
              (vector-set! (gen-tree-children tree) i
                           node)
              old-node])]
      [`(,i ,is ...)
       (insert-node is node (vector-ref (gen-tree-children tree) i))]
      ['() ;; initial tree (or replacement)
       (set! tree-root node)]
      [_ (error "tree didn't have expected generation pattern" loc tree)]))
  (insert-node full-loc node tree-root)
  tree-root)

(define (get-node-at-loc loc tree-root)
  (let recur ([t tree-root]
              [l loc])
    (match l
      [`(,i)
       (if (and (0 . < . (vector-length (gen-tree-children t)))
                (i . < . (vector-length (gen-tree-children t))))
           (vector-ref (gen-tree-children t) i)
           #f)]
      ['() t]
      [`(,i ,is ...)
       (recur (vector-ref (gen-tree-children t) i) is)]
      [_ #f])))

(define (remove-tree-node loc tree-root)
  (let recur ([t tree-root]
              [l loc])
    (match l
      [`(,i)
       (if (= (vector-length (gen-tree-children t)) (+ i 1))
           (set-gen-tree-children! t (vector-take (gen-tree-children t) i))
           (error "can only remove the last child of a node"))]
      [`(,i ,is ...)
       (recur (vector-ref (gen-tree-children t) i) is)]
      [`()
       (error "can't remove tree root")]))
  tree-root)


(define (set-focus-to-true node)
  (match node
    [(gen-tree loc as cs)
     (set-attributes-focus! as #t)]
    [_
     (void)]))

(define (all-trees trace)
  (for/list ([i (length trace)]) (make-tree (take trace i))))
 
(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define green-brush (new brush% [color "Medium Sea Green"]))
(define focus-brush (new brush% [color "Red"]))
(define blue-pen (new pen% [color "Steel Blue"] [width 2]))
(define grey-pen (new pen% [color "Light Slate Grey"] [width 1]))
(define grey-brush (new pen% [color "Slate Grey"]))

(define (add-layout-info tree locs->x-coords)
  (let loop ([t tree]
             [depth 1])
    (match t
      [(gen-tree loc (attributes p b-f b l l2 cs f e) children)
       (for ([c children])
         (loop c (add1 depth)))
       (set-coords-x! cs (hash-ref locs->x-coords loc))
       (set-coords-y! cs depth)]))
  tree)

(define (max-depth trace)
  (define max-d 0)
  (let loop ([t trace])
    (match t
      [`((,loc ,other-stuff ...) ,remaining-traces ...)
       (when (> (add1 (length loc)) max-d)
         (set! max-d (add1 (length loc))))
       (loop remaining-traces)]
      [_ max-d])))

(define (raw-locs trace)
  (map (match-lambda
         [`(,loc ,name ,state ,term ,body ,bound ,depth ,env)
          loc])
       trace))

;; handles keeping track of the trace
;; and mutating the tree
;; tree-frame does the drawing
(define trace-state%
  (class object%
    
    (init trace-init)
    
    (define step 0)
    (define tree #f)
    (define trace trace-init)
    (define tr-locs (raw-locs trace))
    (define-values (locs->widths locs->coords)
      (trace->widths/coords tr-locs))
    (define final-tree (make-tree trace))
    (define width (hash-ref locs->widths '()))
    (define depth (apply max (map length tr-locs)))
    (define last-atts #f)
    (define lvars->names (make-hash))
    (define names-inc 0)
    (define subtree-stack '())
    
    (super-new)
    
    (update-tree-one-step)
    
    (define/public (take-step)
      (unless (equal? step (sub1 (length trace)))
        (set! step (add1 step))
        (update-tree-one-step)))
    
    (define/public (step-back)
      (unless (= step 0)
        (match (first subtree-stack)
          [`(,loc ,subtree)
           (if subtree
             (set! tree (insert-tree-node loc subtree tree))
             (set! tree (remove-tree-node loc tree)))])
        (set! subtree-stack (rest subtree-stack))
        (set! step (sub1 step))
        (defocus)
        (set-focus-to-true (get-node-at-loc (trace-step-loc (list-ref trace step)) tree))
        (set! last-atts (trace-step->atts (list-ref trace step)))
        (add-layout-info tree locs->coords)))
    
    (define/public (rewind)
      (set! step 0)
      (set! tree #f)
      (update-tree-one-step))
    
    (define/public (fast-forward)
      (for ([i (in-range (add1 step) (length trace))])
        (take-step)))
    
    (define/public (get-tree) tree)
    
    (define/public (get-width) width)
    (define/public (get-depth) depth)
    
    (define/public (current-atts)
      last-atts)
    
    (define/public (prettify-pat term)
      (match term
        [`(name ,id ,bound)
         id]
        [`(list ,subterms ...)
         (for/list ([s subterms]) (prettify-pat s))]
        [`(cstr (,nts ...) ,term)
         `(cstr (,@nts) ,(prettify-pat term))]
        [_ term]))
    
    ;; TODO: "garbage collect" vars,
    ;; treating rule and input as roots
    (define/public (format-pattern p eqs)
      (match p
        [`(name ,id ,bound)
         (format-pattern (hash-ref eqs (lvar id)) eqs)]
        [(lvar id)
         (format-pattern (hash-ref eqs (lvar id)) eqs)]
        [`(list ,subterms ...)
         (for/list ([s subterms]) (format-pattern s eqs))]
        [`(cstr (,nts ...) ,p)
         `(cstr (,@nts) ,(format-pattern p eqs))]
        [_ p]))
    
    (define/public (update-focus x y-index)
      (defocus)
      (define as (get-coord-atts x y-index))
      (when as
        (set-attributes-focus! as #t))
      as)
    
    (define/public (get-coord-atts x y-index)
      (define (get-x t-node)
        (match t-node
          [(gen-tree loc (attributes p b-f b l l2 (coords x y) f e) cs)
           x]
          [_ +inf.0]))
      (define closest-node
        (let recur ([t tree]
                    [d (sub1 y-index)])
          (cond
            [(= d 0)
             t]
            [else
             (match t
               [(gen-tree loc as '#())
                #f]
               [(gen-tree loc as children)
                (define c-closest-ns (for/list ([c children])
                                       (recur c (sub1 d))))
                (first (sort c-closest-ns <
                             #:key (λ (c) (if c 
                                              (abs (- x (get-x c)))
                                              +inf.0))))])])))
      (match closest-node 
        [(gen-tree loc as children)
         (set-attributes-focus! as #t)
         as]
        [_ #f]))
    
    (define/public (focus-coords)
      (let recur ([t tree])
        (match t
          [(gen-tree loc (attributes p b-f b l l2 cs #t e) children)
           cs]
          [(gen-tree loc as '#())
           #f]
          [(gen-tree loc as children)
           (define s-t-l (memf (λ (e) e)
                               (for/list ([c children]) (recur c))))
           (if (or (not s-t-l) (empty? s-t-l))
               #f
               (first s-t-l))])))
    
    (define/public (format-info as)
      (match as
        [(attributes name id bound term body coords f e)
         (define eqs (env-eqs e))
         (define sorted-vars (sort (hash-keys eqs)
                                   (λ (l r) (string<? (symbol->string (lvar-id l))
                                                      (symbol->string (lvar-id r))))))
         (string-append
          (string-join
           (for/list ([k sorted-vars])
             (string-append (symbol->string (lvar-id k)) "\t = "
                            (match (hash-ref eqs k)
                              [(lvar next) (symbol->string next)]
                              [`(name ,next ,_) (symbol->string next)]
                              [_ " - "]) "\t = "
                            (string-replace (pretty-format (format-pattern (hash-ref eqs k) eqs)) "\n" "\n\t   \t   ")))
           "\n")
          "\n\n"
          (string-join
           (for/list ([a-dq (in-list (env-dqs e))])
             (match a-dq
               [(dq ps dq-e)
                    (string-append "∀ " (format "~s" ps)
                                   (format "~s" (first dq-e)) " ≠\n\t"
                                   (format "~s" (second dq-e)))])
           "\n")))]))
    
    (define/private (defocus)
      (let recur ([t tree])
        (match t
          [(gen-tree loc as '#())
           (set-attributes-focus! as #f)]
          [(gen-tree loc as children)
           (set-attributes-focus! as #f)
           (for ([c children]) (recur c))])))
    
    (define/private (update-tree-one-step)
      (define trace-step (list-ref trace step))
      (when tree (defocus))
      (match trace-step
        [`(,loc ,name ,state ,term ,body ,bound ,depth ,env)
         (set! subtree-stack (cons (list loc (get-node-at-loc loc tree)) subtree-stack))
         (define atts (attributes name (gensym) (positive? bound) term body (coords #f #f) #t env))
         (set! last-atts atts)
         (set! tree (insert-tree-atts loc atts tree))]
        [_ (error "Trace had incorrect format, failed to update tree")])
      (add-layout-info tree locs->coords))
    
    ))
;; end trace-state%      

  
(define yscale-base .632)
(define (set-y-base f) (set! yscale-base f))
(define (ybase-sum) (/ yscale-base (- 1 yscale-base)))
(define (find-ybase-center)
  (define mid (/ (ybase-sum) 2))
  (define sums (for/hash ([i 10])
                 (values (abs (- mid
                                 (apply + (for/list ([k i]) 
                                            (expt yscale-base i)))))
                         i)))
  (hash-ref sums (apply min (hash-keys sums))))


(define trans-steps 15)
(define (set-t-s x) (set! trans-steps x))

(define Y-SHIFT 0)
(define (set-y-shift s) (set! Y-SHIFT s))

(define tree-canvas%
  (class canvas%
    
    (super-new)
    
    (define scroll-handler (λ (e) #f))
    (define/public (set-scroll-handler f)
      (set! scroll-handler f))
    (define/override (on-scroll event)
      (scroll-handler event))
    
    (define key-handler (λ (e) #f))
    (define/public (set-key-handler f)
      (set! key-handler f))
    (define/override (on-char event)
      (key-handler event))))

(define tree-frame%
  (class frame%
    
    (init t w h)
    (define trace (new trace-state% [trace-init t]))
    (define width w)
    (define height h)
    (define shift (/ w 2))
    
    (super-new [label "Generation Trace"])
    
    (define (t-width)
      (send trace get-width))
    (define (depth)
      (send trace get-depth))
    
    (define y-index 1)
    (define x-coord 0)
    (define scale 1)
    (define/private (rescale factor)
      (set! scale (* scale factor)))
    (define/private (trans-x x)
      (set! x-coord (+ x-coord x))
      (update-scroll-x x-coord))
    (define/private (shift-y steps)
      (set! y-index (+ y-index steps))
      (update-scroll-y y-index))
    
    (define rescale-factor (/ w (t-width)))
    (define scale-factor (if (= 0 (depth))
                             1
                             (expt rescale-factor (/ 1 (depth)))))
    
    (rescale rescale-factor)
    (define canvas (new tree-canvas% [parent this]
                        [min-width w]
                        [min-height h]
                        [style (list 'hscroll 'vscroll)]
                        [paint-callback (lambda (canvas dc)
                                          (draw-t))]))
    (send canvas set-scroll-range 'horizontal SCROLL-RANGE)
    (send canvas set-scroll-range 'vertical SCROLL-RANGE)
    (send canvas set-scroll-pos 'vertical 0)
    (send canvas set-scroll-pos 'horizontal (/ SCROLL-RANGE 2))
    
    (send canvas set-scroll-handler
          (λ (event)
            (define dir (send event get-direction))
            (define pos (update-scroll-pos event))
            (define x-pos x-coord)
            (define y-pos y-index)
            (match dir
              ['horizontal
               (set! x-pos (* (t-width) (- .5 (/ pos SCROLL-RANGE))))]
              ['vertical
               (set! y-pos (- (* (depth) (/ pos SCROLL-RANGE))))])
            (animate-transition (- x-pos x-coord) (- y-pos y-index))))
    
    (send canvas set-key-handler
          (λ (event)
            (define-values (d-x d-y)
              (match (send event get-key-code)
                ['wheel-down
                 (values 0 (- (/ (depth) 40)))]
                ['wheel-up
                 (values 0 (/ (depth) 40))]
                ['wheel-right
                 (values (- (/ (t-width) 40)) 0)]
                ['wheel-left
                 (values (/ (t-width) 40) 0)]
                ['left
                 (send trace step-back)
                 (update/all-steps)
                 (values #f #f)]
                ['right
                 (send trace take-step)
                 (update/all-steps)
                 (values #f #f)]
                [_
                 (values #f #f)]))
            (when (and d-x d-y)
              (animate-transition d-x d-y))))
    
    (define/private (update-scroll-pos event)
      (define pos (send event get-position))
      (match (send event get-event-type)
        ['line-up
         (- pos (/ SCROLL-RANGE 20))]
        ['line-down
         (+ pos (/ SCROLL-RANGE 20))]
        ['page-up
         (- pos (/ SCROLL-RANGE 20))]
        ['page-down
         (+ pos (/ SCROLL-RANGE 20))]
        [_ pos]))
    
    (define/private (update-scroll-x x)
      (send canvas set-scroll-pos 'horizontal (+ (* (- x) (/ SCROLL-RANGE (t-width))) 
                                                 (/ SCROLL-RANGE 2))))
    (define/private (update-scroll-y y-index)
      (send canvas set-scroll-pos 'vertical (max 0 (* (/ y-index (- (max 1 (depth)))) SCROLL-RANGE))))
                                          
    (define dc (send canvas get-dc))
    
    (define/public (display)
      (send this show #t))
    
    (define panel1 (new vertical-panel% [parent this]
                             [min-height 80]
                             [stretchable-height 80]))
    (define panel2 (new horizontal-panel% [parent panel1]
                             [min-height 40]
                             [stretchable-height 40]))
    (define rule-message (new text-field% [parent panel2]
                              [label "Rule:"]))
    (define rewind-button (new button% [parent panel2]
                               [label "<<"]
                               [callback (λ (b e)
                                           (send trace rewind)
                                           (update/all-steps))]))
    (define back-button (new button% [parent panel2]
                             [label "Back"]
                             [callback (λ (b e)
                                         (send trace step-back)
                                         (update/all-steps))]))
    (define step-button (new button% [parent panel2]
                             [label "Step"]
                             [callback (λ (b e)
                                         (send trace take-step)
                                         (update/all-steps))]))
    (define ff-button (new button% [parent panel2]
                           [label ">>"]
                           [callback (λ (b e)
                                       (send trace fast-forward)
                                       (update/all-steps))]))
    (define term-message (new text-field% [parent panel1]
                              [label "Term:"]))
    (define body-message (new text-field% [parent panel1]
                              [label "Body:"]))
    (update-messages)
    
    (define/private (update/all-steps)
      (define f-coords (send trace focus-coords))
      (cond
        [f-coords
         (define center-x (x-inv-map shift))
         (define ∆x (- center-x (coords-x f-coords)))
         (define ∆y (- 5 (+ y-index (coords-y f-coords))))
         (animate-transition ∆x ∆y)]
        [else
         (send canvas refresh-now
               (λ (dc) (draw-t)))])
      (update-messages))
    
    (define/private (update-messages [atts #f])
      (define as (if atts atts (send trace current-atts)))
      (match as
        [(attributes name id bound term body coords f e)
         (send rule-message set-value (format "~s" name))
         (send term-message set-value (format "~s" (send trace prettify-pat term)))
         (send body-message set-value (format "~s" (send trace prettify-pat body)))])
      (send panel1 refresh))
    
    (define/private (pop-info-window as)
      (define info-w (new frame% [label "Environment"]
                          [width 300]
                          [height 300]))
      (define output-string (send trace format-info as))
      (define env-text (new text-field% [label #f]
                            [parent info-w]
                            [style '(multiple)]
                            [init-value output-string]))
      (send info-w show #t))
    
    (define y-scale (/ h (ybase-sum)))
    (define y-cs (for/list ([y 10])
                   (* y-scale
                      (apply + (for/list ([i (in-range 1 y)]) (expt yscale-base i))))))
    (define/private (i-for-y y-raw)
      (define y (- y-raw Y-SHIFT))
      (define diffs (for/hash ([i 10] [y-c y-cs]) 
                      (values (+ 0.0 (abs (- y y-c))) i)))
      (hash-ref diffs (+ 0.0 (apply min (hash-keys diffs)))))
    
    (define/private (delta-x x)
      (/ (- (/ width 2) x) scale))
    (define/private (x-inv-map x)
      (- (/ (- x shift) scale) x-coord))
    
    (define/override (on-subwindow-event receiver event)
      (cond 
        [(and (eq? receiver canvas)
              (send event button-down?))
         (define x (send event get-x))
         (define y (send event get-y))
         (define ∆x (delta-x x))
         (define actual-y (- (i-for-y y) y-index))
         (define actual-x (x-inv-map x))
         (cond 
           [(send event button-down? 'left)
            (define focus-as (send trace update-focus actual-x (round actual-y)))
            (update-messages focus-as)
            (send canvas refresh-now
                  (λ (dc) (draw-t)))]
           [(send event button-down? 'right)
            (pop-info-window (send trace get-coord-atts actual-x (round actual-y)))])]
        [else
         #f]))
    
    (define/private (animate-transition ∆x ∆y)
      (define scaling (expt scale-factor ∆y))
      (define trans-steps (inexact->exact (ceiling (max (abs (/ (* ∆x 40) (t-width)))
                                                        (if (= 0 (depth)) 0 (abs (/ (* ∆y 40) (depth))))
                                                        1))))
      (define dx (/ ∆x trans-steps))
      (define dy (/ ∆y trans-steps))
      (define ds (expt scaling (/ 1 trans-steps)))
      (for ([i trans-steps])
        (trans-x dx)
        (shift-y dy)
        (rescale ds)
        (send canvas refresh-now
              (λ (dc) (draw-t)))))
    
    (define map-y-memo (make-hash))
    (define map-y-int-memo (make-hash))
    (define/private (map-y-int y)
      (hash-ref map-y-int-memo y
                (λ ()
                  (define res
                    (if (< 0 y)
                        (+ Y-SHIFT (* (apply + (for/list ([i (in-range 1 y)]) 
                                                 (expt yscale-base i)))
                                      y-scale))
                        (- Y-SHIFT (* (+ (abs y) 1) y-scale))))
                  (hash-set! map-y-int-memo y res)
                  res)))
    (define/private (map-y y)
      (hash-ref map-y-memo y
                (λ ()
                  (define y-t (truncate y))
                  (define res (if (= y y-t)
                                  (map-y-int y)
                                  (let ([frac (abs (- y y-t))]
                                        [next (if (<= 0 y-t) (+ y-t 1) (- y-t 1))])
                                    (+ (* frac (map-y-int next))
                                       (* (- 1 frac) (map-y-int y-t))))))
                  (hash-set! map-y-memo y res)
                  res)))
    
    (define/private (adjust-x x)
      (* scale (+ x-coord x)))
    (define/private (adjust-y y)
      (+ y y-index))
    
    (define/private (line x1-raw y1-raw x2-raw y2-raw)
      (define x1 (adjust-x x1-raw))
      (define x2 (adjust-x x2-raw))
      (define y1 (adjust-y y1-raw))
      (define y2 (adjust-y y2-raw))
      (when (or (and (x1 . > . (- shift)) (x1 . < . shift) 
                     (y1 . > . -1) (y1 . < . 10))
                (and (x2 . > . (- shift)) (x2 . < . shift) 
                     (y2 . > . -1) (y2 . < . 10)))
        (send dc set-brush no-brush)
        (send dc set-pen blue-pen)
        (send dc draw-line (+ shift x1) (map-y y1) (+ shift x2) (map-y y2))))
    
    (define/private (node x-raw y-raw focus?)
      (define x (adjust-x x-raw))
      (define y (adjust-y y-raw))
      (when (and (x . > . (- shift)) (x . < . shift)
                 (y . < . 8) (y . > . -1))
        (send dc set-pen no-pen)
        (cond
          [focus?
           (send dc set-brush focus-brush)
           (send dc draw-ellipse (+ shift (- x 7)) (- (map-y y) 7) 14 14)]
          [else
           (send dc set-brush green-brush)
           (send dc draw-ellipse (+ shift (- x 5)) (- (map-y y) 5) 10 10)])))
    
    (define/private (draw-t)
      (define (draw-subtree t d)
        (match t
          [(gen-tree loc (attributes p b-f b l l2 (coords x y) f e) '#())
           (node x y f)]
          [(gen-tree loc (attributes p b-f b l l2 (coords x y) f e) children)
           (for ([c children])
             (match c 
               [(gen-tree loc (attributes p b-f b l l2 (coords c-x c-y) f e) children)
                (line x y c-x c-y)
                (draw-subtree c (add1 d))]))
           (node x y f)]))
      (send dc set-smoothing 'aligned)
      (draw-subtree (send trace get-tree) 0))))

;;; end treeframe%
