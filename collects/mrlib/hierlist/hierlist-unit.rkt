(module hierlist-unit racket/base
  (require racket/class
           racket/unit
           mred/mred-sig
           mrlib/include-bitmap
           "hierlist-sig.rkt")

  ;; Previously was a rename-in from mzlib/list, but
  ;; now it's imported from racket/base
  (define sort* sort)

  (define turn-up (include-bitmap "../../icons/turn-up.png" 'png/mask))
  (define turn-down (include-bitmap "../../icons/turn-down.png" 'png/mask))
  (define turn-up-click (include-bitmap "../../icons/turn-up-click.png" 'png/mask))
  (define turn-down-click (include-bitmap "../../icons/turn-down-click.png" 'png/mask))

  (provide hierlist@)
  (define-unit hierlist@
    (import mred^)
    (export hierlist^)
    (init-depend mred^)

      (define-local-member-name
	;; In hierarchical-list%
	ensure-not-selected)

      (define transparent (make-object brush% "WHITE" 'transparent))
      (define transparent-pen (make-object pen% "WHITE" 1 'transparent))
      (define black-xor-pen (make-object pen% (get-highlight-background-color) 1 'solid))
      (define red (make-object brush% "RED" 'solid))
      (define blue (make-object brush% "BLUE" 'solid))
      (define black-xor (make-object brush% (get-highlight-background-color) 'solid))
      (define arrow-cursor (make-object cursor% 'arrow))

      (define-values (up-bitmap down-bitmap up-click-bitmap down-click-bitmap)
	(values turn-up turn-down turn-up-click turn-down-click))

      ;; Hack for implementing auto-wrapping items:
      (define arrow-size 0)

      (define orig-size (max (send up-click-bitmap get-width) (send up-click-bitmap get-height)))

      ;; Private arrow snip class:
      (define arrow-snip-class (make-object snip-class%))
      (send arrow-snip-class set-classname "hier-arrow")
      (define arrow-snip%
	(class snip%
          (init callback)
          (inherit get-admin set-flags get-flags set-count set-snipclass get-style)
          (rename-super [super-get-extent get-extent])
          (define size-calculated? #f)
          (define size orig-size)
          (define width-fraction 1/2)
          (define on? #f)
          (define click-callback callback)
          (define clicked? #f)
          (private*
            [set-sizes
             (lambda (dc)
               (let* ([s (get-style)]
                      [h (send s get-text-height dc)]
                      [d (send s get-text-descent dc)]
                      [a (send s get-text-space dc)])
                 (set! size (max orig-size (- h d a)))
                 (set! size-calculated? #t)
		 (set! arrow-size size)))]
            [get-width (lambda () size)]
            [get-height (lambda () size)]
            [update
             (lambda ()
               (send (get-admin) needs-update this 0 0 (get-width) (get-height)))])
          (override*
            [get-extent (lambda (dc x y w h descent space lspace rspace)
                          (super-get-extent dc x y w h descent space lspace rspace)
                          (unless size-calculated? (set-sizes dc))
                          (when w (set-box! w (get-width)))
                          (when h (set-box! h (get-height)))
                          (when descent (set-box! descent 0))
                          (when space (set-box! space 0)))]
            [partial-offset (lambda (dc x y len)
                              (unless size-calculated? (set-sizes dc))
                              (if (zero? len)
                                  0 
                                  (get-width)))]
            [draw (lambda (dc x y left top right bottom dx dy draw-caret)
                    (unless size-calculated? (set-sizes dc))
                    (let* ([bitmap (if clicked?
                                       (if on? down-click-bitmap up-click-bitmap)
                                       (if on? down-bitmap up-bitmap))]
                           [bw (send bitmap get-width)]
                           [bh (send bitmap get-height)])
                      (send dc draw-bitmap-section bitmap 
                            (+ x (max 0 (- (/ size 2) (/ bw 2))))
                            (+ y (max 0 (- (/ size 2) (/ bh 2))))
			    0 0 (min bw (+ size 2)) (min bh (+ size 2))
                            'solid
                            (send the-color-database find-color "black")
                            (send bitmap get-loaded-mask))))]
            [size-cache-invalid (lambda () (set! size-calculated? #f))]
            [on-event
             (lambda (dc x y mediax mediay event)
               (let ([in-range?
                      (and (<= 0 (- (send event get-x) x) (get-width))
                           (<= 0 (- (send event get-y) y) (get-height)))])
                 (cond
                   [(send event button-down?)
                    (when in-range?
                      (unless clicked?
                        (set! clicked? #t)
                        (update)))]
                   [(send event button-up?)
                    (when clicked?
                      (set! clicked? #f)
                      (update))
                    (when in-range?
                      (on (not on?))
                      (click-callback this))]
                   [(send event dragging?)
                    (unless (or (and clicked? in-range?)
                                (and (not clicked?) (not in-range?)))
                      (set! clicked? (not clicked?))
                      (update))]
                   [else (when clicked?
                           (set! clicked? #f)
                           (update))])))]
            [copy (lambda () (make-object arrow-snip% click-callback))])
          (public*
            [on (case-lambda 
                 [(v) (set! on? v) (update)]
                 [() on?])])
          
          (super-make-object)
          (set-snipclass arrow-snip-class)
          (set-count 1)
          (set-flags (cons 'handles-events (get-flags)))))

      ;; Hack to get whitespace matching width of arrow: derive a new
      ;; class that overrides the `draw' method to do nothing. 
      (define whitespace-snip%
	(class arrow-snip%
	  (override* [draw (lambda (dc x y left top right bottom dx dy draw-caret) (void))])
          (super-make-object void)))

      ;; Keymap to map clicks and double-clicks
      (define item-keymap (make-object keymap%))

      (send item-keymap add-function "mouse-select"
	    (lambda (edit event) (when (send event button-down?)
                                   (send edit click-select #t)
				   ;; To handle hypertext clicks:
				   (send edit on-default-event event))))
      (send item-keymap add-function "mouse-double-select"
	    (lambda (edit event) (when (send event button-down?)
				   (send edit double-select))))
      
      (send item-keymap map-function "leftbutton" "mouse-select")
      (send item-keymap map-function "leftbuttondouble" "mouse-double-select")

      (define hierarchical-list-item<%>
	(interface ()
	  get-editor 
          is-selected?
          select
          user-data
          get-allow-selection?
          set-allow-selection
          get-clickable-snip
          get-parent))

      (define hierarchical-list-item%
	(class* object% (hierarchical-list-item<%>)
          (init snp)
          (define snip snp)
          (define data #f)
          (define allow-selection #t)
          (public*
            [get-allow-selection? (lambda () allow-selection)]
            [set-allow-selection (lambda (_a) (set! allow-selection _a))]
            
            [get-clickable-snip (lambda () snip)]
            [get-editor (lambda () (send snip get-item-buffer))]
            
            ;; the `get-editor' method is overridden
            [is-selected? (lambda () (send (get-editor) is-selected?))]
            
            [select (lambda (on?) (send snip select on?))]
            [click-select (lambda (on?) (send snip click-select on?))]
            [scroll-to (lambda () (let* ([admin (send snip get-admin)]
                                         [dc (and admin (send admin get-dc))]
                                         [h-box (box 0.0)])
                                    (when dc
                                      (send snip get-extent dc 0 0 #f h-box #f #f #f #f)
                                      (send admin
                                            scroll-to
                                            snip
                                            0 0 0 (unbox h-box) #t))))]
            [user-data (case-lambda [() data][(x) (set! data x)])]
            [get-parent (lambda () 
                          (let ([parent-of-snip (send snip get-parent)])
                            (and parent-of-snip
                                 (let ([parent-snip (send parent-of-snip get-parent-snip)])
                                   (and parent-snip
                                        (send parent-snip get-item))))))])
          (super-make-object)))

      (define hierarchical-list-compound-item<%>
	(interface (hierarchical-list-item<%>)
	  new-item 
          new-list
          delete-item
          get-items
          open
          close
          toggle-open/closed
          is-open?
          get-arrow-snip))

      (define hierarchical-list-compound-item%
	(class* hierarchical-list-item% (hierarchical-list-compound-item<%>)
          (init snp)
          (define snip snp)
          (override*
            [get-editor (lambda () (send snip get-title-buffer))])
          (public*
            [get-arrow-snip (lambda () (send snip get-arrow-snip))]
            [open
             (lambda ()
               (send snip open))]
            [close
             (lambda ()
               (send snip close))]
            [toggle-open/closed
             (lambda ()
               (send snip toggle-open/closed))]
            [is-open?
             (lambda ()
               (send snip is-open?))]
            [new-item 
             (lambda x
               (begin0
                 (send (send snip get-content-buffer) new-item . x)
                 (send snip not-empty-anymore)))]
	    [set-no-sublists (lambda x (send (send snip get-content-buffer) set-no-sublists . x))]
            [new-list 
             (lambda x
               (begin0 
                 (send (send snip get-content-buffer) new-list . x)
                 (send snip not-empty-anymore)))]
            [delete-item (lambda (i) (begin0
                                       (send (send snip get-content-buffer) delete-item i)
                                       (send snip check-empty-now)))]
            [get-items (lambda () (send (send snip get-content-buffer) get-items))])
          (super-make-object snip)))

      ;; Buffer for a single list item
      (define hierarchical-item-text%
	(class text%
          (init tp tp-select itm snp dpth)
          (inherit hide-caret
                   last-position set-position set-keymap
                   invalidate-bitmap-cache set-max-width
                   get-view-size)
          (rename-super [super-auto-wrap auto-wrap]
                        [super-on-default-event on-default-event])
          
          (define top tp)
          (define top-select tp-select)
          (define item itm)
          (define snip snp)
          (define depth dpth)
          (define selected? #f)
          (public*
            [is-selected? (lambda () selected?)]
            [show-select (lambda (on?)
                           (set! selected? on?)
                           (invalidate-bitmap-cache))])
          (override*
            [auto-wrap (case-lambda
                        [() (super-auto-wrap)]
                        [(on?) (super-auto-wrap on?)
                         (when on?
                           (let ([wbox (box 0)])
                             (send (send top get-editor) get-view-size wbox (box 0))
                             ;; These icky constants should be eliminated
                             (let ([w (- (unbox wbox) 8 (* depth arrow-size))])
                               (set-max-width (if (positive? w)
                                                  w
                                                  'none)))))])]
            [refresh (lambda (x y width height draw-caret background)
                       (super refresh x y width height
                              (if (and selected?
                                       (or (not (send top show-focus))
                                           (send top has-focus?)))
                                  (cons 0 1)
                                  draw-caret)
                              background))]
            [on-paint
             (lambda (pre? dc left top_ right bottom dx dy caret)
               (when (and pre? selected?)
                 (let ([b (send dc get-brush)]
                       [p (send dc get-pen)]
                       [filled? (or (not (send top show-focus))
                                    (send top has-focus?))])
                   (unless filled?
                     ;; To draw the right outline, we need the display area
                     (set! left 0)
                     (set! top_ 0)
                     (let ([wbox (box 0)]
                           [hbox (box 0)])
                       (get-view-size wbox hbox)
                       (set! right (unbox wbox))
                       (set! bottom (unbox hbox))))
                   (send dc set-brush (if filled? black-xor transparent))
                   (send dc set-pen (if filled? transparent-pen black-xor-pen))
                   (send dc draw-rectangle (+ dx left) (+ dy top_) (- right left) (- bottom top_))
		   (unless (or filled? ((- right left) . < . 2) ((- bottom top_) . < . 2))
		     (send dc draw-rectangle (+ dx left 1) (+ dy top_ 1) (- right left 2) (- bottom top_ 2)))
                   (send dc set-pen p)
                   (send dc set-brush b))))])
	  (private*
            ;; need to use top-select anyway, because it might want to react to
            ;; all clicks
	    [do-select (lambda (on? clicked?)
			 (top-select (if on? item #f) snip clicked?
                                     (not (eq? (not selected?) (not on?)))))])
          (public*
            [select (lambda (on?) (do-select on? #f))]
            [click-select (lambda (on?) (do-select on? #t))]
            [double-select (lambda () (send top on-double-select item))]
            [select-prev (lambda () (send top select-prev))])
          (override*
            [on-default-char (lambda (x) (void))]
	    [can-do-edit-operation? (lambda (x [r? #t]) 
				      (and (super can-do-edit-operation? x r?)
					   (send top can-do-edit-operation? x r?)))]
	    [do-edit-operation (lambda (x [r? #t] [time 0]) (send top do-edit-operation x r? time))])
          (super-make-object)
          (hide-caret #t)
          (set-keymap item-keymap)))
      
      ;; Buffer for a compound list item (and the top-level list)
      (define (make-hierarchical-list-text% super%)
	(class super%
          (init tp tp-select dpth parent-snp)
          (inherit hide-caret erase
                   last-position insert delete line-start-position line-end-position
                   begin-edit-sequence end-edit-sequence get-style-list)
          (define top tp)
          (define top-select tp-select)
          (define depth dpth)
          (define parent-snip parent-snp)
          (define children null)
	  (define new-children null)
	  (define no-sublists? #f)
          (define transparent? #f)
          (private*
            [append-children! (lambda ()
                                (unless (null? new-children)
                                  (set! children (append children (reverse new-children)))
                                  (set! new-children null)))]
            [make-whitespace (lambda () (make-object whitespace-snip%))]
            [insert-item 
             (lambda (mixin snip% whitespace?)
               (let ([s (make-object snip% this top top-select (add1 depth) mixin)])
                 (send s use-style-background transparent?)
                 (begin-edit-sequence)
                 (unless (and (null? children)
                              (null? new-children))
                   (insert #\newline (last-position)))
                 (when whitespace?
                   (insert (make-whitespace) (last-position)))
                 (insert s (last-position))
                 (end-edit-sequence)
                 (set! new-children (cons s new-children))
                 (send s get-item)))])
          (public*
            [set-transparent (λ (t?) (set! transparent? (and t? #t)))]
            [get-parent-snip (lambda () parent-snip)]
            [deselect-all 
             (lambda () 
               (append-children!)
               (for-each (lambda (x) (send x deselect-all)) children))]
            [new-item 
             (case-lambda
              [() (new-item (lambda (x) x))]
              [(mixin)
               (insert-item mixin hierarchical-item-snip% (not no-sublists?))])]
            [new-list
             (case-lambda
              [() (new-list (lambda (x) x))]
              [(mixin)
	       (when no-sublists?
		 (error 'new-list "this list has been designated with `set-no-sublists' as having no sublists"))
               (insert-item mixin hierarchical-list-snip% #f)])]
	    [set-no-sublists
	     (lambda (no?)
               (append-children!)
	       (unless (null? children)
		 (error 'set-no-sublists "cannot change sublist mode because the list is non-empty"))
	       (set! no-sublists? (and no? #t)))]
            [get-items (lambda () 
                         (append-children!)
                         (map (lambda (x) (send x get-item)) children))]
            [delete-item
             (lambda (i)
               (append-children!)
               (let loop ([pos 0][l children][others null])
                 (cond
                   [(null? l) (error 'hierarchical-list-compound-item::delete-item "item not found: ~a" i)]
                   [(eq? (send (car l) get-item) i)
		    (send top ensure-not-selected i)
                    (send (car l) deselect-all)
                    (set! children (append (reverse others) (cdr l)))
                    (let ([s (line-start-position pos)]
                          [e (line-end-position pos)])
                      (delete (if (zero? s) s (sub1 s)) (if (zero? s) (add1 e) e)))]
                   [else (loop (add1 pos) (cdr l) (cons (car l) others))])))]
            [sort (lambda (less-than? [recur? #t])
                    (append-children!)
                    (let ([l (sort* children
                                    (lambda (a b)
                                      (less-than? (send a get-item)
                                                  (send b get-item))))])
                      (begin-edit-sequence)
		      (when recur?
			(for-each (lambda (child)
				    (when (is-a? child hierarchical-list-snip%)
				      (let ([ed (send child get-content-buffer)])
					(when (is-a? ed hierarchical-list-text%)
					  (send ed sort less-than?)))))
				  children))
                      (erase)
                      (let ([to-scroll-to #f])
                        (for-each
                         (lambda (s)
                           (unless to-scroll-to
                             (when (and (is-a? (send s get-item) hierarchical-list-item<%>)
                                        (send (send s get-item) is-selected?))
                               (set! to-scroll-to s)))
                           (unless (or no-sublists?
				       (is-a? s hierarchical-list-snip%))
                             (insert (make-whitespace)))
                           (insert s)
                           (insert #\newline))
                         l)
                        (unless (null? l)
                          (delete)) ; delete last #\newline
                        (set! children l)
                        (when to-scroll-to
                          (send (send to-scroll-to get-item) scroll-to)))
                      (end-edit-sequence)))]
            [reflow-items
             (lambda ()
               (append-children!)
               (for-each
                (lambda (c)
                  (send c reflow-item))
                children))])
          (override*
            [on-default-char (lambda (x) (void))]
            [on-default-event (lambda (x) (void))]
	    [can-do-edit-operation? (lambda (x [r? #t]) 
				      (and (super can-do-edit-operation? x r?)
					   (send top can-do-edit-operation? x r?)))]
	    [do-edit-operation (lambda (x [r? #t] [time 0]) (send top do-edit-operation x r? time))])
          (super-make-object)
          (hide-caret #t)))

      (define hierarchical-list-text% (make-hierarchical-list-text% text%))

      ;; Snip for a single list item
      (define hierarchical-item-snip%
	(class editor-snip%
          (init prnt top top-select depth mixin)
          (define parent prnt)
	  (public*
	    [get-parent (lambda () parent)]
	    [get-item-text% (lambda () hierarchical-item-text%)]
	    [select (lambda (on?) (send item-buffer select on?))]
	    [click-select (lambda (on?) (send item-buffer click-select on?))]
	    [deselect-all (lambda () (select #f))]
	    [show-select (lambda (on?) (send item-buffer show-select on?))]
	    [get-item-buffer (lambda () item-buffer)]
	    [get-item (lambda () item)]
	    [reflow-item (lambda () 
			   (when (send item-buffer auto-wrap)
			     (send item-buffer auto-wrap #t)))])
          
	  (define item (make-object (mixin hierarchical-list-item%) this))
	  (define item-buffer (make-object (get-item-text%) top top-select item this depth))
	  (super-make-object item-buffer #f 0 0 0 0 0 0 0 0)))

      ;; Snip for a compound list item
      (define hierarchical-list-snip%
        (class editor-snip%
          (init prnt tp top-select depth mixin [title #f][content #f])
          (define parent prnt)
          (define top tp)
	  (public*
	    [get-parent (lambda () parent)]
	    [get-main-text% (lambda () (class text%
                                         (init-rest args)
					 (override*
					   [on-default-char (lambda (x) (void))]
					   [on-default-event (lambda (x) (void))]
					   [can-do-edit-operation? (lambda (x [r? #t]) 
								     (and (super can-do-edit-operation? x r?)
									  (send top can-do-edit-operation? x r?)))]
					   [do-edit-operation (lambda (x [r? #t] [time 0]) (send top do-edit-operation x r? time))])
                                         (apply super-make-object args)))]
	    [get-title-text% (lambda () hierarchical-item-text%)]
	    [get-content-text% (lambda () hierarchical-list-text%)]
	    [get-arrow-snip% (lambda () arrow-snip%)]
	    [select (lambda (on?) (send title-buffer select on?))]
	    [click-select (lambda (on?) (send title-buffer click-select on?))]
	    [deselect-all (lambda ()
			    (select #f)
			    (send content-buffer deselect-all))]
	    [show-select (lambda (on?) (send title-buffer show-select on?))]
	    [not-empty-anymore (lambda ()
				 (when was-empty?
				   (set! was-empty? #f)
				   (set! was-non-empty? #t)
				   (send main-buffer begin-edit-sequence)
				   (send main-buffer insert #\newline 2)
				   (send main-buffer insert whitespace 3)
				   (send main-buffer insert content-snip 4)
				   (send main-buffer end-edit-sequence)))]
	    [check-empty-now (lambda ()
			       (when (and was-non-empty? 
					  (zero? (send content-buffer last-position)))
				 (set! was-empty? #t)
				 (set! was-non-empty? #f)
				 (send main-buffer delete 2 5)))]
	    [open (lambda () (handle-open #t))]
	    [close (lambda () (handle-close #t))]
	    [is-open? (lambda () open?)]
	    [toggle-open/closed
	     (lambda ()
	       (if open?
		   (handle-close #t)
		   (handle-open #t)))]
	    [on-arrow (lambda (a)
			(if (send a on)
			    (handle-open #f)
			    (handle-close #f)))]
	    [get-title-buffer (lambda () title-buffer)]
	    [get-content-buffer (lambda () content-buffer)]
	    [get-item (lambda () item)]
	    [reflow-item (lambda () 
			   (when (send title-buffer auto-wrap)
			     (send title-buffer auto-wrap #t))
			   (send (send content-snip get-editor) reflow-items))])
          (define open? #f)
	  (private*
	    [handle-open
	     (lambda (update-arrow?)
	       (unless open?
		 (set! open? #t)
		 (when update-arrow? (send arrow on #t))
		 (send main-buffer begin-edit-sequence)
		 (send top on-item-opened (get-item))
		 (if (zero? (send content-buffer last-position))
		     (set! was-empty? #t)
		     (begin
		       (set! was-non-empty? #t)
		       (send main-buffer insert #\newline 2)
		       (send main-buffer insert whitespace 3)
		       (send main-buffer insert content-snip 4)))
		 (send main-buffer scroll-to-position 0
		       #f
		       (send main-buffer last-position)
		       'start)
		 (send main-buffer end-edit-sequence)))]
	    [handle-close
	     (lambda (update-arrow?)
	       (when open?
		 (set! open? #f)
		 (when update-arrow? (send arrow on #f))
		 (set! was-empty? #f)
		 (set! was-non-empty? #f)
		 (send main-buffer begin-edit-sequence)
		 (send content-buffer deselect-all)
		 (send main-buffer delete 2 5)
		 (send top on-item-closed (get-item))
		 (send main-buffer end-edit-sequence)))])
	  (define was-empty? #f)
	  (define was-non-empty? #f)
	  (define item (make-object (mixin hierarchical-list-compound-item%) this))
	  (define main-buffer (make-object (get-main-text%)))
	  (define title-buffer (make-object (get-title-text%) top top-select item this depth))
	  (define content-buffer (make-object (get-content-text%) top top-select depth this))
	  (define title-snip (make-object editor-snip% title-buffer #f 0 0 0 0 0 0 0 0))
	  (define content-snip (make-object editor-snip% content-buffer #f 4 0 0 0 0 0 0 0))
	  (define arrow (make-object (get-arrow-snip%) (lambda (a) (on-arrow a))))
	  (define whitespace (make-object whitespace-snip%))
          (override*
            [use-style-background
             (λ (x)
               (super use-style-background x)
               (send title-snip use-style-background x)
               (send content-snip use-style-background x)
               (send content-buffer set-transparent x))])
          (public*
            [get-arrow-snip (lambda () arrow)])
          (inherit style-background-used?)
	  (super-make-object main-buffer #f 0 0 0 0 0 0 0 0)
          (send main-buffer hide-caret #t)
	  (send main-buffer insert arrow)
	  (when title (send title-buffer insert title))
	  (when content (send content-buffer insert content))
	  (send main-buffer insert title-snip)
	  (send main-buffer change-style (make-object style-delta% 'change-alignment 'top) 0 2)))

      (define list-keymap (make-object keymap%))

      (send list-keymap add-function "select-in"
	    (lambda (list event) (send list select-in)))
      (send list-keymap add-function "select-out"
	    (lambda (list event) (send list select-out)))
      (send list-keymap add-function "select-prev"
	    (lambda (list event) (send list select-prev)))
      (send list-keymap add-function "select-next"
	    (lambda (list event) (send list select-next)))
      (send list-keymap add-function "select-first"
	    (lambda (list event) (send list select-first)))
      (send list-keymap add-function "select-last"
	    (lambda (list event) (send list select-last)))
      (send list-keymap add-function "page-up"
	    (lambda (list event) (send list page-up)))
      (send list-keymap add-function "page-down"
	    (lambda (list event) (send list page-down)))

      (send list-keymap map-function "right" "select-in")
      (send list-keymap map-function "left" "select-out")
      (send list-keymap map-function "up" "select-prev")
      (send list-keymap map-function "down" "select-next")
      (send list-keymap map-function "home" "select-first")
      (send list-keymap map-function "end" "select-last")
      (send list-keymap map-function "pageup" "page-up")
      (send list-keymap map-function "pagedown" "page-down")
      (send list-keymap map-function "a:up" "page-up")
      (send list-keymap map-function "a:down" "page-down")
      (send list-keymap map-function "m:up" "page-up")
      (send list-keymap map-function "m:down" "page-down")
      (send list-keymap map-function "d:up" "select-first")
      (send list-keymap map-function "d:down" "select-last")
      (send list-keymap map-function "esc;v" "page-up")
      (send list-keymap map-function "c:v" "page-down")
      (send list-keymap map-function "esc;>" "select-last")
      (send list-keymap map-function "esc;<" "select-first")
      
      (send list-keymap add-function "toggle-open/closed"
	    (lambda (list event) (send list toggle-open/closed)))
      (send list-keymap map-function "return" "toggle-open/closed")

      (define hierarchical-list%
        (class editor-canvas%
          (init parent [style '(no-hscroll)])
          (inherit min-width min-height allow-tab-exit refresh)
          (rename-super [super-on-char on-char]
                        [super-on-focus on-focus])
          (public*
            [selectable
             (case-lambda
              [() selectable?]
              [(on?) (set! selectable? on?)])]
            [get-selected (lambda () selected-item)]
            [on-item-opened (lambda (i) (void))]
            [on-item-closed (lambda (i) (void))]
            [on-double-select (lambda (i) (void))]
            [on-select (lambda (i) (void))]
            [on-click (lambda (i) (void))]
            [new-item (lambda x (send top-buffer new-item . x))]
            [set-no-sublists (lambda x (send top-buffer set-no-sublists . x))]
            [new-list (lambda x (send top-buffer new-list . x))]
            [delete-item (lambda (i) (send top-buffer delete-item i))]
            [sort (lambda (less-than? [recur? #t]) (send top-buffer sort less-than? recur?))]
            [get-items (lambda () (send top-buffer get-items))]
            [toggle-open/closed
             (lambda ()
               (cond
                 [(and selected (is-a? selected hierarchical-list-snip%))
                  (send selected toggle-open/closed)]
                 [else
                  (void)]))]
            [select-out (lambda () 
                          (when selected
                            (let* ([parent-snip (send (send selected get-parent) get-parent-snip)])
                              (cond
                                [parent-snip
                                 (let ([parent (send parent-snip get-item)])
                                   (when (send parent get-allow-selection?)
                                     (send parent click-select #t)
                                     (send parent scroll-to)))]
                                [else
                                 (void)]))))]
            [select-in (lambda () 
                         (cond
                           [(and selected (is-a? selected hierarchical-list-snip%)) 
                            (let ([edit-sequence-text (send selected get-editor)])
                              (send edit-sequence-text begin-edit-sequence)
                              (send selected open)
                              (let ([items (send selected-item get-items)])
                                (unless (null? items)
                                  (send (car items) click-select #t)
                                  (send (car items) scroll-to)))
                              (send edit-sequence-text end-edit-sequence))]
                           [else (void)]))]
            [select-next (lambda () (move +1))]
            [select-prev (lambda () (move -1))]
            [select-first (lambda () (let ([l (get-items)])
                                       (unless (null? l)
                                         (send (car l) click-select #t)
                                         (send (car l) scroll-to))))]
            [select-last (lambda () (let loop ([l (get-items)])
                                      (cond
                                        [(null? l) (void)]
                                        [(null? (cdr l))
                                         (send (car l) click-select #t)
                                         (send (car l) scroll-to)]
                                        [else (loop (cdr l))])))]
	    [select (lambda (i) 
                      (cond
                       [i
                        (send i select #t)
                        (send i scroll-to)]
                       [(allow-deselect)
                        (when selected
                          (send selected show-select #f)
                          (set! selected #f)
                          (set! selected-item #f))]
                       [else
                        (error 'hierarchical-list%::select 
                               "can only pass #f when allow-deselect has been called with #t")]))]
            [click-select (lambda (i) 
			    (send i click-select #t)
			    (send i scroll-to))]
            [page-up (lambda () (page 'up))]
            [page-down (lambda () (page 'down))]
            [show-focus
             (case-lambda
              [() show-focus?]
              [(on?) (set! show-focus? on?)])]
	    [can-do-edit-operation? (lambda (x [r? #t]) #f)]
	    [do-edit-operation (lambda (x [r? #t] [time 0]) (void))])
	  (public* ;; ---- local to this module! ----
	    [ensure-not-selected (lambda (i)
				   (when (eq? i selected)
				     (set! selected #f))
				   (when (eq? i selected-item)
				     (set! selected-item #f)))])
          (override*
            [on-char
             (lambda (e)
               (unless (send list-keymap handle-key-event this e)
                 (super-on-char e)))]
            [on-size
             (lambda (w h)
               (send top-buffer begin-edit-sequence)
               (send top-buffer reflow-items)
               (send top-buffer end-edit-sequence))]
            [on-focus
             (lambda (on?)
               (when (and selected show-focus?)
                 (send selected show-select #t))
               (super-on-focus on?))])
          (private*
            [move (lambda (dir) 
                    (define (find i l)
                      (let loop ([l l][pos 0])
                        (if (null? l)
                            #f
                            (if (eq? (car l) i)
                                pos
                                (loop (cdr l) (add1 pos))))))
                    (define (find-next-selectable-item i vec)
                      (let loop ([pos (+ i dir)])
                        (cond
                          [(= pos -1)
                           i]
                          [(= pos (vector-length vec))
                           i]
                          [(send (vector-ref vec pos) get-allow-selection?)
                           pos]
                          [else (loop (+ pos dir))])))
                    ;; Scrolling works differently depending on whether selections
                    ;;  are involved:
                    (if selectable?
                        (let* ([l (if selected
                                      (send (send selected get-parent) get-items)
                                      (get-items))]
                               [vec (list->vector l)]
                               [pos (let ([found (find selected-item l)])
                                      (if (and selected-item found)
                                          (find-next-selectable-item found vec)
                                          (if (negative? dir)
                                              (sub1 (length l))
                                              0)))])
                          (when (< -1 pos (length l))
                            (let ([i (vector-ref vec pos)])
                              (when (send i get-allow-selection?)
                                (send i click-select #t)
                                (send i scroll-to)))))
                        (let ([y-box (box 0.0)]
                              [x-box (box 0.0)]
                              [w-box (box 0.0)]
                              [h-box (box 0.0)])
                          (send (send top-buffer get-admin) get-view x-box y-box w-box h-box)
                          (let ([y (if (negative? dir)
                                       (- (unbox y-box) 2)
                                       (+ (unbox y-box) (unbox h-box) 1))])
                            (send (send top-buffer get-admin) scroll-to
                                  (unbox x-box) y
                                  (unbox w-box) 1)))))]
            [page (lambda (dir)
                    ;; Scrolling works differently depending on whether selections
                    ;;  are involved:
                    (if selectable?
                        (let ([items (get-items)])
                          (unless (null? items)
                            (let ([sbox (box 0)]
                                  [ebox (box 0)])
                              (send top-buffer get-visible-line-range sbox ebox)
                              (let* ([len (max 1 (sub1 (- (unbox ebox) (unbox sbox))))]
                                     [l (if (eq? dir 'up)
                                            (max 0 (- (unbox sbox) len))
                                            (min (sub1 (length items)) (+ (unbox ebox) len)))]
                                     [i (list-ref items l)])
                                (send i click-select #t)
                                (send i scroll-to)))))
                        (send top-buffer move-position dir #f 'page)))])
          (define selectable? #t)
          (define show-focus? #f)
	  (define on-select-always? #t)
	  (define on-click-always? #f)
	  (define allow-deselect? #f)
	  (public*
	    [on-select-always
	     (case-lambda 
	      [() on-select-always?]
	      [(v) (set! on-select-always? (and v #t))])]
	    [on-click-always
	     (case-lambda 
	      [() on-click-always?]
	      [(v) (set! on-click-always? (and v #t))])]
	    [allow-deselect
	     (case-lambda 
	      [() allow-deselect?]
	      [(v) (set! allow-deselect? (and v #t))])])
          (private*
            [do-select (lambda (item s clicked? selected?)
                         (when (and item clicked? on-click-always?)
                           (on-click item))
                         (cond
                           [(not selected?)
                            ;; this wasn't really a selection, only useful if
                            ;; on-click-always? made us do an on-click above
                            (void)]
                           [(and selectable?
                                 item
                                 (send item get-allow-selection?))
                            (unless (eq? item selected-item)
                              (when selected (send selected show-select #f))
                              (set! selected (if item s #f))
                              (set! selected-item item)
                              (when selected (send selected show-select #t))
			      (when (or clicked? on-select-always?)
				(on-select item)))]
                           [(and item clicked?)
                            (unless on-click-always? ; already called above
                              (on-click item))]
			   [allow-deselect?
			    (when selected-item
			      (send selected show-select #f)
			      (set! selected #f)
			      (set! selected-item #f))
			    (when (or clicked? on-select-always?)
			      (on-select #f))]))])
          (define top-buffer (make-object hierarchical-list-text% this (lambda (i s c? s?) (do-select i s c? s?)) 0 #f))
          (define selected #f)
          (define selected-item #f)
          (send top-buffer set-transparent (member 'transparent style))
          (super-make-object parent top-buffer style)
	  (allow-tab-exit #t)
          (send top-buffer set-cursor arrow-cursor) 
          (min-width 150)
          (min-height 200)))))
