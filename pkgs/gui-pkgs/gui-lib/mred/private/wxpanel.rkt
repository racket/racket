(module wxpanel racket/base
  (require racket/class
           racket/list
           (prefix-in wx: "kernel.rkt")
           "lock.rkt"
           "const.rkt"
           "helper.rkt"
           "check.rkt"
           "wx.rkt"
           "wxwindow.rkt"
           "wxitem.rkt"
           "wxcontainer.rkt")

  (provide (protect-out wx-panel%
                        wx-vertical-panel%
                        wx-vertical-tab-panel%
                        wx-vertical-group-panel%
                        wx-horizontal-panel%
                        wx-control-horizontal-panel%
                        wx-pane%
                        wx-vertical-pane%
                        wx-horizontal-pane%
                        wx-grow-box-pane%
                        wx-canvas-panel%
                        wx-vertical-canvas-panel%
                        wx-horizontal-canvas-panel%))

  (define wx:windowless-panel%
    (class object%
      (init prnt x y w h style label)
      (define-values (pos-x pos-y width height parent)
        (values 0 0 1 1 prnt))
      (public*
       [drag-accept-files (lambda () (void))]
       [on-drop-file (lambda () (void))]
       [on-set-focus (lambda () (void))]
       [on-kill-focus (lambda () (void))]
       [set-focus (lambda () (void))]
       [gets-focus? (lambda () #f)]
       [enable (lambda () (void))]
       [show (lambda (on?) (void))]
       [is-shown? (lambda () #f)]
       [is-shown-to-root? (lambda () (send parent is-shown-to-root?))]
       [is-enabled-to-root? (lambda () (send parent is-enabled-to-root?))]
       [get-parent (lambda () parent)]
       [get-client-size (lambda (wb hb)
                          (when wb (set-box! wb width))
                          (when hb (set-box! hb height)))]
       [skip-enter-leave-events (lambda (skip?) (void))]
       [set-size (lambda (x y w h) 
                   (unless (negative? x) (set! pos-x x))
                   (unless (negative? y) (set! pos-y y))
                   (unless (negative? w) (set! width w))
                   (unless (negative? h) (set! height h)))]
       [get-x (lambda () pos-x)]
       [get-y (lambda () pos-y)]
       [set-event-positions-wrt (lambda (c) (void))]
       [get-width (lambda () width)]
       [get-height (lambda () height)]
       [adopt-child (lambda (c) (send (get-parent) adopt-child c))])
      (super-make-object)))

  (define tab-h-border (if (eq? (system-type) 'unix)
			   2
			   3))
  (define tab-v-bottom-border (if (memq (system-type) '(macosx macos))
				  0
				  2))

  (define (wx-make-basic-panel% wx:panel% stretch? [x-m 0] [y-m 0])
    (class* (wx-make-container% (make-item% wx:panel% x-m y-m stretch? stretch?)) (wx-basic-panel<%>)
      (init parent style label)
      (inherit get-x get-y get-width get-height
               min-width min-height set-min-width set-min-height
               x-margin y-margin
               get-client-size area-parent
               get-hard-minimum-size
               get-top-level)
	       
      (rename-super [super-set-focus set-focus])
	       
      ;; cache to prevent on-size from recomputing its result every
      ;; time. when curr-width is #f, cache invalid.
      (define curr-width (void))
      (define curr-height (void))
		
      ;; list of child-info structs corresponding to the children.  (#f
      ;;  if no longer valid.)
      (define children-info null)
		
      ;; Not used by linear panels
      (define h-align 'center)
      (define v-align 'center)

      ;; Needed for windowless panes
      (define move-children? #f)

      (define ignore-redraw-request? #f)


      (define auto-scroll-x? (and (memq 'auto-hscroll style) #t))
      (define auto-scroll-y? (and (memq 'auto-vscroll style) #t))

      (define can-scroll-x? (or auto-scroll-x?
                                (and (memq 'hscroll style) #t)))
      (define can-scroll-y? (or auto-scroll-y?
                                (and (memq 'vscroll style) #t)))
                
      (define scroll-x? can-scroll-x?)
      (define scroll-y? can-scroll-y?)
	       
      (override*
       [has-tabbing-children? (lambda () #t)]

       [set-focus                    ; dispatch focus to a child panel
        (lambda ()
          (if (focus-on-self?)
              (super-set-focus)
              (if (null? children)
                  (super-set-focus)
                  (send (car children) set-focus))))]

       [ext-dx (lambda () (if hidden-child
                              tab-h-border
                              0))]
       [ext-dy (lambda () (if hidden-child
                              (let-values ([(mw mh) (get-hard-minimum-size)])
                                (- mh tab-v-bottom-border 1))
                              0))])
	       
      ;; list of panel's contents.
      (define children null)
      (define hidden-child #f)
      (define curr-border const-default-border)
      (define border? (memq 'border style))
	       
      (public*
       [need-move-children (lambda () (set! move-children? #t))]
       [focus-on-self? (lambda () #f)]

       [get-children (lambda () children)]
       [get-hidden-child (lambda () hidden-child)]
       [set-first-child-is-hidden (lambda ()
                                    (set! hidden-child (car children))
                                    (let ([i (send hidden-child get-info)])
                                      (set-min-width (child-info-x-min i))
                                      (set-min-height (child-info-y-min i))))])
      
      (override*
       [ensure-forgotten (lambda ()
                           (for ([c (in-list children)])
                             (send c ensure-forgotten))
                           (super ensure-forgotten))])

      (public*         
       [border
        (case-lambda
          [() curr-border]
          [(new-val)
           (check-margin-integer '(method area-container<%> border) new-val)
           (set! curr-border new-val)
           (force-redraw)])]

       ;; add-child: adds an existing child to the panel.
       ;; input: new-child: item% descendant to add
       ;; returns: nothing
       ;; effects: adds new-child to end of list of children.
       [add-child
        (lambda (new-child)
          (unless (eq? this (send new-child area-parent))
            (raise-arguments-error 'add-child 
                                   "subwindow is not a child of this container"
                                   "subwindow" (wx->proxy new-child)))
          (when (memq new-child children)
            (raise-arguments-error 'add-child "subwindow area is already active"
                                   "subwindow" (wx->proxy new-child)))
          (change-children
           (lambda (l)
             (append l (list new-child)))))]
		 
       ;; change-children: changes the list of children.
       ;; input: f is a function which takes the current list of children
       ;;   and returns a new list of children.
       ;; returns: nothing
       ;; effects: sets the list of children to the value of applying f.
       [change-children
        (lambda (f)
          (let ([new-children (f children)]) ;; hidden child, if any , must be first!
            (unless (andmap (lambda (child)
                              (eq? this (send child area-parent)))
                            new-children)
              (raise-arguments-error 'change-children
                                     (string-append 
                                      "not all members of the returned list are "
                                      "children of the container")
                                     "container" (wx->proxy this)
                                     "list" (map wx->proxy (remq hidden-child new-children))))
            (let loop ([l new-children])
              (unless (null? l)
                (if (memq (car l) (cdr l))
                    (raise-arguments-error 'change-children 
                                           "child appears  multiple times in the returned list" 
                                           "child" (wx->proxy (car l))
                                           "list" (map wx->proxy (remq hidden-child new-children)))
                    (loop (cdr l)))))
            ;; show all new children, hide all deleted children.
            (let ([added-children (list-diff new-children children)]
                  [removed-children (list-diff children new-children)])
              (let ([non-window (ormap (lambda (child)
                                         (and (not (is-a? child wx:window%))
                                              child))
                                       removed-children)])
                (when non-window
                  (raise-arguments-error 'change-children
                                         "cannot delete non-window area"
                                         "area" non-window
                                         "container" (wx->proxy this))))
              
              ;; Newly-added children may have been removed when
              ;;  disabled, or now added into a disabled panel:
              (for-each (lambda (child) (send child queue-active))
                        added-children)

              (let ([top (get-top-level)])
                (for-each (lambda (child) (send top show-child child #f))
                          removed-children)
                (set! children new-children)
                (force-redraw)
                (for-each (lambda (child) (send top show-child child #t))
                          added-children)))))]
		 
       ;; delete-child: removes a child from the panel.
       ;; input: child: child to delete.
       ;; returns: nothing
       ;; effects: removes child from list; forces redraw.
       [delete-child
        (lambda (child)
          (unless (memq child children)
            (raise-arguments-error 'delete-child 
                                   "subwindow is not a child of this container or child is not active" 
                                   "subwindow" (wx->proxy child)))
          (change-children (lambda (child-list)
                             (remq child child-list))))]
		 
       ;; get-children-info: returns children info list, recomputing it
       ;;   if needed.
       ;; input: none
       ;; returns: list of child-info structs.
       ;; effects: upon exit, children-info is eq? to result.
       [get-children-info
        (lambda ()
          (unless children-info
            (let* ([childs children]
                   [info (map (lambda (child)
                                (send child get-info))
                              childs)])
              (if (and (= (length childs) (length children))
                       (andmap eq? childs children))
                  ;; Got the info for the right set of children
                  (set! children-info info)
			    
                  ;; During the call to some get-info, the set of children changed;
                  ;; try again
                  (get-children-info))))
          children-info)]
		 
       [child-redraw-request
        (lambda (from)
          (unless (or ignore-redraw-request?
                      (not (memq from children)))
            (force-redraw)))]
		 
       ;; do-graphical-size: creates a function which returns the minimum
       ;;   possible size for a horizontal-panel% or vertical-panel% object.
       ;; input: compute-x/compute-y: functions which take the current x/y
       ;;          location, the amount of spacing which will come after the
       ;;          current object, and the list of child-info structs beginning
       ;;          with the current object, and return the new x/y locations.
       ;; returns: a thunk which returns the minimum possible size of the
       ;;   entire panel (not just client) as a list of two elements:
       ;;   (min-x min-y). 
       [do-graphical-size
        (lambda (ignore-scroll? compute-x compute-y)
          (letrec ([gms-help
                    (lambda (kid-info x-accum y-accum first?)
                      (if (null? kid-info)
                          (list x-accum y-accum)
                          (gms-help
                           (cdr kid-info)
                           (if (and can-scroll-x? (not ignore-scroll?))
                               x-accum
                               (compute-x x-accum kid-info (and hidden-child first?)))
                           (if (and can-scroll-y? (not ignore-scroll?))
                               y-accum
                               (compute-y y-accum kid-info (and hidden-child first?)))
                           #f)))])
            (let-values ([(client-w client-h)
                          (get-two-int-values (lambda (a b) (get-client-size a b)))])
              (let* ([border (border)]
                     [min-client-size
                      (gms-help (get-children-info)
                                (* 2 border) (* 2 border)
                                #t)]
                     [delta-w (if ignore-scroll? 0 (- (get-width) client-w))]
                     [delta-h (if ignore-scroll? 0 (- (get-height) client-h))])
                (list (+ delta-w (car min-client-size) (if hidden-child (* 2 tab-h-border) 0))
                      (+ delta-h (cadr min-client-size)))))))]
		 
       ;; do-get-min-graphical-size: poll children and return minimum possible
       ;;   size, as required by the graphical representation of the tree,
       ;;   of the panel.
       ;; input: none
       ;; returns: minimum full size (as a list, width & height) of the
       ;;   container.
       ;; effects: none
       [get-graphical-min-size (lambda () (void))]
       [do-get-graphical-min-size
        (lambda ([ignore-scroll? #f])
          (do-graphical-size 
           ignore-scroll?
           (lambda (x-accum kid-info first?)
             (max x-accum (+ (* 2 (border))
                             (child-info-x-min (car kid-info)))))
           (lambda (y-accum kid-info first?)
             (max y-accum (+ (* 2 (border))
                             (child-info-y-min (car kid-info)))))))])
	       
      (override*
       [force-redraw
        (lambda ()
          (set! children-info #f)
          (set! curr-width #f)
          (let ([parent (area-parent)])
            (send parent child-redraw-request this)))]

       ;; get-min-size: poll children and return minimum possible size
       ;;   for the container which considers the user min sizes.
       ;; input: none
       ;; returns: minimum full size (as a list, width & height) of
       ;;   container.
       ;; effects: none.
       [get-min-size
        (lambda ()
          (let ([graphical-min-size (get-graphical-min-size)])
            (list (+ (* 2 (x-margin))
                     (max (car graphical-min-size) (min-width)))
                  (+ (* 2 (y-margin))
                     (max (cadr graphical-min-size) (min-height))))))]
		 
       [on-container-resize
        (lambda ()
          (let-values ([(client-width client-height)
                        (get-two-int-values (lambda (a b) (get-client-size a b)))])
            (unless (and (number? curr-width)
                         (number? curr-height)
                         (= curr-width client-width)
                         (= curr-height client-height)
                         (not move-children?))
              (set! curr-width client-width)
              (set! curr-height client-height)
              (set! move-children? #f)
              (redraw client-width client-height))))]

       [init-min (lambda (x) (if border? 8 0))])
	       
      (public*
       ;; place-children: determines where each child of panel should be
       ;; placed.
       ;; input: children-info: list of (int int bool bool)
       ;;        width/height: size of panel's client area.
       ;; returns: list of placement info for children; each item in list
       ;;   is a list of 4 elements, consisting of child's x-posn,
       ;;   y-posn, x-size, y-size (including margins).  Items are in same 
       ;;   order as children-info list.
       [place-children (lambda (l w h) (void))]
       [check-place-children
        (lambda (children-info width height)
          (unless (and (list? children-info)
                       (andmap (lambda (x) (and (list? x)
                                                (= 4 (length x))
                                                (integer? (car x)) (not (negative? (car x))) (exact? (car x))
                                                (integer? (cadr x)) (not (negative? (cadr x))) (exact? (cadr x))))
                               children-info))
            (raise-argument-error (who->name '(method area-container-window<%> place-children))
                                  "(listof (list/c exact-nonnegative-integer? exact-nonnegative-integer? any/c any/c))"
                                  children-info))
          (check-non-negative-integer '(method area-container-window<%> place-children) width)
          (check-non-negative-integer '(method area-container-window<%> place-children) height))]
       [do-place-children
        (lambda (children-info width height)
          (check-place-children children-info width height)
          (define m (border))
          (define w (- width (* 2 m)))
          (define h (- height (* 2 m)))
          (let loop ([children-info children-info])
            (if (null? children-info)
                null
                (let ([curr-info (car children-info)])
                  (define cw (car curr-info))
                  (define ch (cadr curr-info))
                  (define w-stretch? (caddr curr-info))
                  (define h-stretch? (cadddr curr-info))
                  (cons
                   (list
                    (if w-stretch?
                        m
                        (case h-align
                          [(center) (quotient (- w cw) 2)]
                          [(right) (- w cw)]
                          [else 0]))
                    (if h-stretch?
                        m
                        (case v-align
                          [(center) (quotient (- h ch) 2)]
                          [(bottom) (- h ch)]
                          [else 0]))
                    (if w-stretch?
                        w
                        cw)
                    (if h-stretch?
                        h
                        ch))
                   (loop (cdr children-info)))))))])

      (define curr-spacing const-default-spacing)
	       
      (public*
       [spacing                         ; does nothing!
        (case-lambda
          [() curr-spacing]
          [(new-val)
           (check-margin-integer '(method area-container<%> spacing) new-val)
           (set! curr-spacing new-val)])]

       [do-align (lambda (h v set-h set-v)
                   (unless (memq h '(left center right))
                     (raise-argument-error 'set-alignment "(or/c 'left 'center 'right)" h))
                   (unless (memq v '(top center bottom))
                     (raise-argument-error 'set-alignment "(or/c 'top 'center 'bottom)" v))
                   (set-h h)
                   (set-v (case v [(top) 'left] [(center) 'center] [(bottom) 'right])))]
       [alignment (lambda (h v) 
                    (do-align h v (lambda (h) (set! h-align h)) (lambda (h) (set! v-align v)))
                    (force-redraw))]
       [get-alignment (lambda () (values h-align v-align))]

       [adjust-panel-size (lambda (w h) 
                            (if (or can-scroll-x? can-scroll-y?)
                                (let ([ms (do-get-graphical-min-size #t)])
                                  ;; loop for fix-point on x and y scroll
                                  (let loop ([w w] [h h] [iters 0])
                                    (let ([want-scroll-x?
                                           (if auto-scroll-x?
                                               ((car ms) . > . w)
                                               scroll-x?)]
                                          [want-scroll-y?
                                           (if auto-scroll-y?
                                               ((cadr ms) . > . h)
                                               scroll-y?)])
                                      (if (and (eq? scroll-x? want-scroll-x?)
                                               (eq? scroll-y? want-scroll-y?))
                                          (values (if can-scroll-x?
                                                      (max w (car ms))
                                                      w)
                                                  (if can-scroll-y?
                                                      (max h (cadr ms))
                                                      h))
                                          (begin
                                            (set! scroll-x? want-scroll-x?)
                                            (set! scroll-y? want-scroll-y?)
                                            (send this show-scrollbars scroll-x? scroll-y?)
                                            (let-values ([(w h)
                                                          (get-two-int-values (lambda (a b) (get-client-size a b)))])
                                              (if (= iters 2)
                                                  (values w h)
                                                  (loop w h (add1 iters)))))))))
                                (values w h)))]

       ;; redraw: redraws panel and all children
       ;; input: width, height: size of area area in panel.
       ;; returns: nothing
       ;; effects: places children at default positions in panel.
       [redraw
        (lambda (in-width in-height)
          (let-values ([(children-info) (get-children-info)]
                       [(children) children] ; keep list of children matching children-info
                       [(width height) (adjust-panel-size in-width in-height)])
            (let ([l (place-children (map (lambda (i)
                                            (list (child-info-x-min i) (child-info-y-min i)
                                                  (child-info-x-stretch i) (child-info-y-stretch i)))
                                          (if hidden-child
                                              (cdr children-info)
                                              children-info))
                                     (if hidden-child
                                         (- width (* 2 tab-h-border))
                                         width)
                                     (if hidden-child
                                         (- height (child-info-y-min (car children-info))) ;; 2-pixel border here, too
                                         height))])
              (define expected-length (- (length children-info) (if hidden-child 1 0)))
              (unless (and (list? l)
                           (= (length l) expected-length)
                           (andmap (lambda (x) (and (list? x)
                                                    (= 4 (length x))
                                                    (andmap exact-integer? x)))
                                   l))
                (raise-arguments-error
                 'container-redraw 
                 (format
                  (string-append 
                   "result from place-children is not a list of length ~a (matching the"
                   " input list length) whose elements are lists of length 4 of exact integers")
                  expected-length)
                 "result" l))
              (panel-redraw children children-info (if hidden-child
                                                       (cons (list 0 0 width height)
                                                             (let ([dy (child-info-y-min (car children-info))])
                                                               (map (lambda (i)
                                                                      (list (+ (car i) tab-h-border)
                                                                            (+ dy (cadr i) (- tab-v-bottom-border) -1)
                                                                            (caddr i)
                                                                            (cadddr i)))
                                                                    l)))
                                                       l)))))]
       [panel-redraw
        (lambda (childs child-infos placements)
          (when (or scroll-y? scroll-x?)
            (let ([w (if scroll-x?
                         (+ (for/fold ([x 0]) ([p (in-list placements)]
                                               [i (in-list child-infos)])
                              (max x (+ (max 0 (car p))
                                        (max (+ (child-info-x-min i)
                                                (* 2 (child-info-x-margin i)))
                                             (caddr p)))))
                            (* 2 (border)))
                         0)]
                  [h (if scroll-y?
                         (+ (for/fold ([y 0]) ([p (in-list placements)]
                                               [i (in-list child-infos)])
                              (max y (+ (max 0 (cadr p))
                                        (max (+ (child-info-y-min i)
                                                (* 2 (child-info-y-margin i)))
                                             (cadddr p)))))
                            (* 2 (border)))
                         0)]
                  [wb (box 0)]
                  [hb (box 0)])
              (get-client-size wb hb)
              (let ([do-x-scroll? (w . > . (unbox wb))]
                    [do-y-scroll? (h . > . (unbox hb))])
                (send this set-scrollbars
                      (if do-x-scroll? 1 0) (if do-y-scroll? 1 0)
                      w h
                      (unbox wb) (unbox hb)
                      -1 -1
                      #t))))
          (for-each
           (lambda (child info placement)
             (let-values ([(x y w h) (apply values placement)])
               (let ([minw (child-info-x-min info)]
                     [minh (child-info-y-min info)]
                     [xm (child-info-x-margin info)]
                     [ym (child-info-y-margin info)])
                 (dynamic-wind
                   (lambda () (set! ignore-redraw-request? #t))
                   (lambda ()
                     (send child set-size
                           (max 0 (+ x xm)) (max 0 (+ y ym))
                           (- (max minw w) (* 2 xm))
                           (- (max minh h) (* 2 ym))))
                   (lambda () (set! ignore-redraw-request? #f)))
                 (send child on-container-resize))))
           childs
           child-infos
           placements))])
      (super-make-object style parent -1 -1
                         (if can-scroll-y? 20 (if can-scroll-x? 1 0))
                         (if can-scroll-x? 20 (if can-scroll-y? 1 0))
                         (cons 'deleted style) label)
      (unless (memq 'deleted style)
        (send (get-top-level) show-control this #t))))

  (define (wx-make-pane% wx:panel% stretch?)
    (class (make-container-glue% (make-glue% (wx-make-basic-panel% wx:panel% stretch?)))
      (init-rest args)
      (inherit get-parent get-x get-y need-move-children get-children)
      (rename-super [super-set-size set-size])
      (override*
       [on-visible
        (lambda ()
          (for-each (lambda (c) (send c queue-visible)) (get-children)))]
       [on-active
        (lambda ()
          (for-each (lambda (c) (send c queue-active)) (get-children)))]

       [get-window (lambda () (send (get-parent) get-window))]
       [set-size (lambda (x y w h) 
                   (super-set-size x y w h)
                   (need-move-children))]
       [dx (lambda () (get-x))]
       [dy (lambda () (get-y))])
      (apply super-make-object args)))

  (define (wx-make-panel% wx:panel% [x-m 0] [y-m 0])
    (class (make-container-glue% (make-window-glue% (wx-make-basic-panel% wx:panel% #t x-m y-m)))
      (init-rest args)
      (rename-super [super-on-visible on-visible]
	            [super-on-active on-active])
      (inherit get-children)
      (override*
       [on-visible
        (lambda ()
          (for-each (lambda (c) (send c queue-visible)) (get-children))
          (super-on-visible))]
       [on-active
        (lambda ()
          (for-each (lambda (c) (send c queue-active)) (get-children))
          (super-on-active))])
      (apply super-make-object args)))

  (define (wx-make-linear-panel% wx-panel%)
    (class wx-panel%
      (init-rest args)
      (define major-align-pos 'left)
      (define minor-align-pos 'center)
      
      (inherit force-redraw border get-width get-height
               do-get-graphical-min-size)
      (define curr-spacing const-default-spacing)
      (override*
       [spacing
        (case-lambda
	  [() curr-spacing]
	  [(new-val)
	   (check-margin-integer '(method area-container<%> spacing) new-val)
	   (set! curr-spacing new-val)
	   (force-redraw)])])
      (public*
       [minor-align (lambda (a) (set! minor-align-pos a) (force-redraw))]
       [major-align (lambda (a) (set! major-align-pos a) (force-redraw))]
       [major-offset (lambda (space)
                       (case major-align-pos
                         [(center) (quotient space 2)]
                         [(left) 0]
                         [(right) space]))]
       [minor-offset (lambda (width size)
                       (case minor-align-pos
                         [(center) (quotient (- width size) 2)]
                         [(left) 0]
                         [(right) (- width size)]))]
	
       [do-get-alignment (lambda (pick) (values (pick major-align-pos minor-align-pos)
                                                (case (pick minor-align-pos major-align-pos)
                                                  [(left) 'top] [(center) 'center] [(right) 'bottom])))]

       ;; place-linear-children: implements place-children functions for
       ;; horizontal-panel% or vertical-panel% classes.
       ;; input: child-major-size: function which takes a child-info struct
       ;;          and returns the child's minimum size in the major direction
       ;;          of the panel.
       ;;        child-major-stretch: function which takes a child-info
       ;;          struct and returns the child's stretchability in the major
       ;;          direction of the panel.
       ;;        child-minor-size/child-minor-stretch: see above.
       ;;        major-dim/minor-dim: functions which take the width and the
       ;;          height of the panel and return the panel's major and minor
       ;;          dimensions, respectively.
       ;;        get-h-info/get-v-info: functions which take info lists
       ;;          describing the major and minor directions and select the
       ;;          appropriate one.
       ;; returns: a function which takes the children info, the width and the
       ;;   height of the panel's client and returns a list which contains
       ;;   posn&size info for each child. 
       [place-linear-children
        (lambda (kid-info width height
                          child-major-size
                          child-major-stretch
                          child-major-offset
                          child-minor-size
                          child-minor-stretch
                          child-minor-position
                          major-dim minor-dim
                          get-x-info get-y-info)
          (letrec ([count-stretchable
                    (lambda (kid-info)
                      (if (null? kid-info)
                          0
                          (let ([curr-info (car kid-info)])
                            (if (child-major-stretch curr-info)
                                (add1 (count-stretchable (cdr kid-info)))
                                (count-stretchable (cdr kid-info))))))])
            (let* ([spacing (spacing)]
                   [border (border)]
                   [num-stretchable (count-stretchable kid-info)]
                   [extra-space (max 0
                                     (- (major-dim width height)
                                        (apply 
                                         major-dim
                                         (do-get-graphical-min-size #t))))]
                   [extra-per-stretchable (if (zero? num-stretchable)
                                              0
                                              (inexact->exact
                                               (floor
                                                (/ extra-space
                                                   num-stretchable))))]
                   [leftover (- extra-space (* extra-per-stretchable num-stretchable))]
                   [num-children (length kid-info)]
                   [major-offset (if (= num-stretchable 0)
                                     (child-major-offset extra-space)
                                     0)])
              (letrec
                  ([pc-help
                    (lambda (kid-info left-edge leftover)
                      (if (null? kid-info)
                          null
                          (let* ([curr-info (car kid-info)]
                                 [rest (cdr kid-info)]
                                 [major-posn left-edge]
                                 [next-leftover (if (zero? leftover)
                                                    0
                                                    (- leftover 1))]
                                 [extra-this-stretchable (if (zero? leftover)
                                                             extra-per-stretchable
                                                             (+ extra-per-stretchable 1))]
                                 [major-size
                                  (if (child-major-stretch curr-info)
                                      (+ extra-this-stretchable
                                         (child-major-size curr-info))
                                      (child-major-size curr-info))]
                                 [minor-posn (if (child-minor-stretch
                                                  curr-info)
                                                 border
                                                 (inexact->exact
                                                  (round
                                                   (child-minor-position 
                                                    (minor-dim width height) 
                                                    (child-minor-size curr-info)))))]
                                 [minor-size (if (child-minor-stretch
                                                  curr-info)
                                                 (- (minor-dim width height)
                                                    (* 2 border))
                                                 (child-minor-size
                                                  curr-info))])
                            (cons
                             (list
                              (get-x-info major-posn minor-posn)
                              (get-y-info major-posn minor-posn)
                              (get-x-info major-size minor-size)
                              (get-y-info major-size minor-size))
                             (pc-help rest
                                      (+ major-size major-posn spacing)
                                      next-leftover)))))])
                (pc-help kid-info (+ border major-offset) leftover)))))])
      
      (apply super-make-object args)))

  
  (define (wx-make-horizontal/vertical-panel% wx-linear-panel% init-horizontal?)
    (class wx-linear-panel%
      (init-rest args)
      (inherit major-align minor-align do-align do-get-alignment major-offset minor-offset
	       spacing border do-graphical-size place-linear-children check-place-children
               force-redraw)
      (define horizontal? init-horizontal?)
      (public* [get-orientation (λ () horizontal?)]
               [set-orientation (λ (h?) 
                                   (unless (equal? h? horizontal?)
                                     (set! horizontal? h?)
                                     (force-redraw)))])
      (override*
       [alignment (lambda (h v) 
                    (if horizontal?
                        (do-align h v 
                                  (lambda (x) (major-align x)) 
                                  (lambda (x) (minor-align x)))
                        (do-align h v 
                                  (lambda (x) (minor-align x)) 
                                  (lambda (x) (major-align x)))))]
       [get-alignment (λ () (do-get-alignment (if horizontal? (λ (x y) x) (λ (x y) y))))]
        
       [do-get-graphical-min-size
        (lambda ([ignore-scroll? #f])
          (if horizontal?
              (do-graphical-size 
               ignore-scroll?
               (lambda (x-accum kid-info hidden?)
                 (+ x-accum (child-info-x-min (car kid-info))
                    (if (or hidden? (null? (cdr kid-info)))
                        0
                        (spacing))))
               (lambda (y-accum kid-info hidden?)
                 (max y-accum
                      (+ (child-info-y-min (car kid-info))
                         (* 2 (border))))))
              (do-graphical-size
               ignore-scroll?
               (lambda (x-accum kid-info hidden?)
                 (max x-accum
                      (+ (child-info-x-min (car kid-info))
                         (* 2 (border)))))
               (lambda (y-accum kid-info hidden?)
                 (+ y-accum (child-info-y-min (car kid-info))
                    (if (or (null? (cdr kid-info)) hidden?)
                        0
                        (spacing)))))))]
 
       [do-place-children
        (lambda (l w h)
          (cond
           [horizontal?
            (check-place-children l w h)
            (place-linear-children l w h
                                   car   ; child-info-x-min
                                   caddr ; child-info-x-stretch
                                   (lambda (s) (major-offset s))
                                   cadr   ; child-info-y-min
                                   cadddr ; child-info-y-stretch
                                   (lambda (s t) (minor-offset s t))
                                   (lambda (width height) width)
                                   (lambda (width height) height)
                                   (lambda (major minor) major)
                                   (lambda (major minor) minor))]
           [else
            (check-place-children l w h)
            (place-linear-children l w h
                                   cadr   ; child-info-y-min
                                   cadddr ; child-info-y-stretch
                                   (lambda (s) (major-offset s))
                                   car   ; child-info-x-min
                                   caddr ; child-info-x-stretch
                                   (lambda (s t) (minor-offset s t))
                                   (lambda (width height) height)
                                   (lambda (width height) width)
                                   (lambda (major minor) minor)
                                   (lambda (major minor) major))]))])
      (apply super-make-object args)))
  
  
  ;; horizontal-panel%: a panel which arranges its children in an evenly
  ;; spaced horizontal row.  Items are vertically centered (or stretched
  ;; to fit the dialog box if they are stretchable).  The items are evenly
  ;; spaced horizontally, with any extra space divided evenly among the
  ;; stretchable items. 
  (define (wx-make-horizontal-panel% wx-linear-panel%) (wx-make-horizontal/vertical-panel% wx-linear-panel% #t))

  ;; vertical-panel%.  See horizontal-panel%, but reverse
  ;; "horizontal" and "vertical."
  (define (wx-make-vertical-panel% wx-linear-panel%) (wx-make-horizontal/vertical-panel% wx-linear-panel% #f))

  (define (wx-make-tab% %)
    (class %
      (inherit gets-focus?)
      (super-new)
      (define/override (tabbing-position x y w h)
        ;; claim that the panel is short and starts above its client area:
        (list this x (- y 16) w 16))
      (define/override (focus-on-self?) (gets-focus?))))

  (define wx-panel% (wx-make-panel% wx:panel%))
  (define wx-control-panel% (wx-make-panel% wx:panel% const-default-x-margin const-default-y-margin))
  (define wx-canvas-panel% (wx-make-panel% wx:canvas-panel%))
  (define wx-tab-panel% (wx-make-tab% (wx-make-panel% wx:tab-panel%)))
  (define wx-group-panel% (wx-make-panel% wx:group-panel%))
  (define wx-linear-panel% (wx-make-linear-panel% wx-panel%))
  (define wx-linear-canvas-panel% (wx-make-linear-panel% wx-canvas-panel%))
  (define wx-control-linear-panel% (wx-make-linear-panel% wx-control-panel%))
  (define wx-linear-tab-panel% (wx-make-tab% (wx-make-linear-panel% wx-tab-panel%)))
  (define wx-linear-group-panel% (wx-make-linear-panel% wx-group-panel%))
  (define wx-horizontal-panel% (wx-make-horizontal-panel% wx-linear-panel%))
  (define wx-vertical-panel% (wx-make-vertical-panel% wx-linear-panel%))
  (define wx-vertical-tab-panel% (wx-make-tab% (wx-make-vertical-panel% wx-linear-tab-panel%)))
  (define wx-vertical-group-panel% (wx-make-vertical-panel% wx-linear-group-panel%))
  (define wx-control-horizontal-panel% (wx-make-horizontal-panel% wx-control-linear-panel%))
  (define wx-horizontal-canvas-panel% (wx-make-horizontal-panel% wx-linear-canvas-panel%))
  (define wx-vertical-canvas-panel% (wx-make-vertical-panel% wx-linear-canvas-panel%))

  (define wx-pane% (wx-make-pane% wx:windowless-panel% #t))
  (define wx-grow-box-pane%
    (class (wx-make-pane% wx:windowless-panel% #f)
      (init mred proxy parent style label)
      (override*
       [init-min (lambda (x) (if (wx:needs-grow-box-spacer?)
                                 15
                                 0))])
      (super-make-object mred proxy parent style label)))
  (define wx-linear-pane% (wx-make-linear-panel% wx-pane%))
  (define wx-horizontal-pane% (wx-make-horizontal-panel% wx-linear-pane%))
  (define wx-vertical-pane% (wx-make-vertical-panel% wx-linear-pane%)))
