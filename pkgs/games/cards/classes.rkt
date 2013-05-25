(module classes racket/base
  (require racket/class
           (prefix-in mred: racket/gui)
           (prefix-in util: "utils.rkt")
           "constants.rkt"
           "make-cards.rkt"
           "region.rkt"
           string-constants
           "../show-help.rkt"
           "../show-scribbling.rkt")

  (provide pasteboard%
	   table%)

  (define pasteboard%
    (class mred:pasteboard%
      (inherit begin-edit-sequence end-edit-sequence get-admin
	       invalidate-bitmap-cache
	       find-next-selected-snip find-first-snip find-snip
	       set-before set-after
	       add-selected is-selected? no-selected set-selected remove-selected
	       get-snip-location move-to
	       dc-location-to-editor-location
	       set-selection-visible)
      
      (define select-one? #t)
      (define select-backward? #f)
      (define raise-to-front? #f)
      (define button-map '((left #f #f #t)
                           (middle #t #f #t)
                           (right #f #t #f)))
      
      (define do-on-double-click 'flip)
      (define do-on-single-click void)
      
      (define selecting? #f)
      (define dragging? #f)
      (define bg-click? #f)
      (define click-base #f)
      (define last-click #f)
      (define regions null)
      (private*
	[get-snip-bounds
	 (lambda (s)
	   (let ([xbox (box 0)]
		 [ybox (box 0)])
	     (get-snip-location s xbox ybox #f)
	     (let ([l (unbox xbox)]
		   [t (unbox ybox)])
	       (get-snip-location s xbox ybox #t)
	       (values l t (unbox xbox) (unbox ybox)))))]
	[for-each-selected
	 (lambda (f)
	   (let loop ([snip (find-next-selected-snip #f)])
	     (when snip
	       (f snip)
	       (loop (find-next-selected-snip snip)))))]
	[make-overlapping-list
	 (lambda (s so-far behind?)
	   (let-values ([(sl st sr sb) (get-snip-bounds s)])
	     (let loop ([t (find-first-snip)][so-far so-far][get? (not behind?)])
	       (cond
		[(not t) so-far]
		[(eq? s t) (if behind?
			       (loop (send t next) so-far #t)
			       so-far)]
		[get?
		 (let ([l (if (and (not (memq t so-far))
				   (let-values ([(tl tt tr tb) 
						 (get-snip-bounds t)])
				     (and (or (<= sl tl sr)
					      (<= sl tr sr))
					  (or (<= st tt sb)
					      (<= st tb sb)))))
			      (make-overlapping-list t (cons t so-far) behind?)
			      so-far)])
		   (loop (send t next) l #t))]
		[else
		 (loop (send t next) so-far #f)]))))]
	[get-reverse-selected-list
	 (lambda ()
	   (let loop ([s (find-next-selected-snip #f)][l null])
	     (if s
		 (loop (find-next-selected-snip s) (cons s l))
		 l)))]
	[shuffle
	 (lambda (selected-list) ; cards to shuffle in back->front order
	   (let* ([permuted-list
		   (util:shuffle-list selected-list 7)]
		  [get-pos
		   (lambda (s)
		     (let ([xb (box 0)]
			   [yb (box 0)])
		       (get-snip-location s xb yb)
		       (cons (unbox xb) (unbox yb))))]
		  [sel-loc-list (map get-pos selected-list)]
		  [perm-loc-list (map get-pos permuted-list)])
	     (for-each
	      (lambda (s start-pos end-pos)
		(let* ([sx (car start-pos)]
		       [sy (cdr start-pos)]
		       [ex (car end-pos)]
		       [ey (cdr end-pos)]
		       [steps (max 1 (floor (/ 50 (length selected-list))))])
		  (let loop ([i 1])
		    (unless (> i steps)
		      (let ([x (+ sx (* (/ i steps) (- ex sx)))]
			    [y (+ sy (* (/ i steps) (- ey sy)))])
			(move-to s x y)
			(mred:flush-display)
			(loop (add1 i)))))))
	      permuted-list perm-loc-list sel-loc-list)
	     (let loop ([l permuted-list])
	       (unless (null? l)
		 (set-before (car l) #f)
		 (loop (cdr l))))
	     (no-selected)))]
	[update-region 
	 (lambda (region)
	   (let-values ([(sx sy sw sh) (get-region-box region)])
	     (invalidate-bitmap-cache sx sy sw sh)))])
      (public*
        [only-front-selected
         (lambda ()
           (let loop ([s (find-next-selected-snip #f)][ok (find-first-snip)])
             (when s
               (if (eq? s ok)
                   (loop (find-next-selected-snip s)
                         (send ok next))
                   (let loop ([s s][l (list s)])
                     (let ([next (find-next-selected-snip s)])
                       (if next
                           (loop next (cons s l))
                           (for-each (lambda (s)
                                       (remove-selected s))
                                     l))))))))])
      (override*
	[on-paint
	 (lambda (before? dc l t r b dx dy caret)
	   (when before?
	     (for-each
	      (lambda (region)
		(when (region-paint-callback region)
		  (let-values ([(sx sy sw sh) (get-region-box region)])
		    ((region-paint-callback region) dc (+ dx sx) (+ dy sy) sw sh)))
		(when (region-label region)
		  (let ([old-b (send dc get-brush)]
			[old-p (send dc get-pen)])
		    (let-values ([(sx sy sw sh) (get-region-box region)])
		      (send dc set-brush white-brush)
		      (send dc set-pen no-pen)
		      (send dc draw-rectangle (+ dx sx) (+ dy sy) sw sh)
		      (send dc set-pen dark-gray-pen)
		      (draw-roundish-rectangle dc (+ dx sx) (+ dy sy) sw sh)
		      (let ([text (region-label region)])
			(if (string? text)
			    (let ([old-f (send dc get-font)])
			      (send dc set-font nice-font)
			      (let-values ([(x y d a) (send dc get-text-extent text)])
				(send dc draw-text text
				      (+ dx sx (/ (- sw x) 2))
				      (if (region-button? region)
					  ;; Since we use size-in-pixels, the letters
					  ;;  should really be 12 pixels high (including
					  ;;  the descender), but the space above the letter
					  ;;  can vary by font; center on 12, splitting
					  ;;  the difference for the descender
					  (+ dy sy (/ (- sh 12) 2) (- 12 y (/ d -2)))
					  (+ dy sy 5))))
			      (send dc set-font old-f))
			    (send dc draw-bitmap text
				  (+ dx sx (/ (- sw (send text get-width)) 2))
				  (+ dy sy (/ (- sh (send text get-height)) 2))
				  'solid black-color
				  (send text get-loaded-mask))))
		      (when (region-hilite? region)
			(send dc set-brush hilite-brush)
			(send dc set-pen no-pen)
			(send dc draw-rectangle (+ dx sx 1) (+ dy sy 1) (- sw 2) (- sh 2))))
		    (send dc set-brush old-b)
		    (send dc set-pen old-p))))
	      regions)))])
      (augment*
	[after-select
	 (lambda (s on?)
	   (inner (void) after-select s on?)
	   (unless (or (not on?) selecting?)
	     (set! selecting? #t)
	     (if select-one?
		 (when raise-to-front?
		   (set-before s #f))
		 (begin
		   (begin-edit-sequence)
		   (let ([l (make-overlapping-list s (list s) select-backward?)])
		     (for-each (lambda (i) (add-selected i)) l))
		   (when raise-to-front?
		     (let loop ([snip (find-next-selected-snip #f)][prev #f])
		       (when snip
			 (if prev
			     (set-after snip prev)
			     (set-before snip #f))
			 (loop (find-next-selected-snip snip) snip))))
		   (end-edit-sequence)))
	     (set! selecting? #f)))]
	[on-interactive-move
	 (lambda (e)
	   (inner (void) on-interactive-move e)
	   (for-each (lambda (region) (set-region-decided-start?! region #f)) regions)
	   (for-each-selected (lambda (snip) (send snip remember-location this)))
	   (set! dragging? #t))])
      (override*
	[interactive-adjust-move
	 (lambda (snip xb yb)
	   (super interactive-adjust-move snip xb yb)
	   (let-values ([(l t r b) (get-snip-bounds snip)])
	     (let-values ([(rl rt rw rh)
			   (let ([r (send snip stay-in-region)])
			     (if r
				 (values (region-x r) (region-y r)
					 (region-w r) (region-h r))
				 (let ([wb (box 0)][hb (box 0)])
				   (send (get-admin) get-view #f #f wb hb)
				   (values 0 0 (unbox wb) (unbox hb)))))])
	       (let ([max-x (- (+ rl rw) (- r l))]
		     [max-y (- (+ rt rh) (- b t))])
		 (when (< (unbox xb) rl)
		   (set-box! xb rl))
		 (when (> (unbox xb) max-x)
		   (set-box! xb max-x))
		 (when (< (unbox yb) rt)
		   (set-box! yb rt))
		 (when (> (unbox yb) max-y)
		   (set-box! yb max-y))))))])
      (augment*
	[after-interactive-move
	 (lambda (e)
           (when dragging?
             (set! dragging? #f)
             (inner (void) after-interactive-move e)
             (for-each-selected (lambda (snip) (send snip back-to-original-location this)))
             (let ([cards (get-reverse-selected-list)])
               (only-front-selected) ; in case overlap changed
               (for-each
                (lambda (region)
                  (when (region-hilite? region)
                    (mred:queue-callback
                                        ; Call it outside the current edit sequence
                     (lambda ()
                       ((region-callback region) cards)
                       (unhilite-region region)))))
                regions))))])
      (override*
	[on-default-event
	 (lambda (e)
	   (let ([click (let ([c (or (and (send e button-down? 'left) 'left)
                                     (and (send e button-down? 'right) 'right)
                                     (and (send e button-down? 'middle) 'middle))])
                          (cond
                           [(eq? c last-click) c]
                           [(not last-click) c]
                           ;; Move/drag event has different mouse button,
                           ;; and there was no mouse up. Don't accept the
                           ;; click, yet.
                           [else #f]))])
             (set! last-click click)
	     (when click
	       (let* ([actions (cdr (assoc click button-map))]
		      [one? (list-ref actions 0)]
		      [backward? (list-ref actions 1)]
		      [raise? (list-ref actions 2)])
		 (unless (and (eq? backward? select-backward?)
			      (eq? one? select-one?)
			      (eq? raise? raise-to-front?))
		   (set! select-one? one?)
		   (set! select-backward? backward?)
		   (set! raise-to-front? raise?)
		   (no-selected))))
	     (let*-values ([(lx ly) (dc-location-to-editor-location 
				     (send e get-x) 
				     (send e get-y))]
			   [(s) (find-snip lx ly)])
	       ; Clicking on a "selected" card unselects others
	       ; in this interface
	       (when (send e button-down?)
		 (unless (or (not click-base) (not s) (eq? s click-base))
		   (no-selected))
		 (set! click-base s))
	       (when (and dragging? click-base (send click-base user-can-move))
		 (for-each
		  (lambda (region)
		    (when (and (not (region-button? region))
			       (region-callback region)
			       (or (not (region-decided-start? region))
				   (region-can-select? region)))
		      (let-values ([(sx sy sw sh) (get-region-box region)])
			(let ([in? (and (<= sx lx (+ sx sw))
					(<= sy ly (+ sy sh)))])
			  (unless (region-decided-start? region)
			    (set-region-decided-start?! region #t)
			    (set-region-can-select?! region (not in?)))
			  (when (and (not (eq? in? (region-hilite? region)))
				     (region-can-select? region))
			    (set-region-hilite?! region in?)
			    (when (region-interactive-callback region)
			      ((region-interactive-callback region) in? (get-reverse-selected-list)))
			    (invalidate-bitmap-cache sx sy sw sh))))))
		  regions))
	       ; Can't move => no raise, either
	       (unless (or (not click-base) (send click-base user-can-move))
		 (set! raise-to-front? #f))
	       (let ([was-bg? bg-click?])
		 (if (send e button-down?)
		     (set! bg-click? (not s))
		     (when (and bg-click? (not (send e dragging?)))
		       (set! bg-click? #f)))
		 (unless bg-click?
		   (super on-default-event e))
                 (when (and bg-click? dragging?)
                   ;; We didn't call super on-default-event, so we need
                   ;;  to explicitly end the drag:
                   (after-interactive-move e))
		 (when bg-click?
		   ; Check for clicking on a button region:
		   (for-each
		    (lambda (region)
		      (when (and (region-button? region)
				 (region-callback region))
			(let-values ([(sx sy sw sh) (get-region-box region)])
			  (let ([in? (and (<= sx lx (+ sx sw))
					  (<= sy ly (+ sy sh)))])
			    (unless (region-decided-start? region)
			      (set-region-decided-start?! region #t)
			      (set-region-can-select?! region in?))
			    (when (and (not (eq? in? (region-hilite? region)))
				       (region-can-select? region))
			      (set-region-hilite?! region in?)
			      (invalidate-bitmap-cache sx sy sw sh))))))
		    regions))
		 (when (and was-bg? (not bg-click?))
		   ; Callback hilighted button:
		   (for-each
		    (lambda (region)
		      (when (region-button? region) 
			(set-region-decided-start?! region #f)
			(when (region-hilite? region)
			  (mred:queue-callback
			   ; Call it outside the current edit sequence
			   (lambda ()
			     ((region-callback region))
			     (unhilite-region region))))))
		    regions)))
	       (when (and (send e button-down?)
			  click-base
			  (not (send click-base user-can-move)))
		 (no-selected)))
	     (when (and click click-base)
	       (do-on-single-click click-base))))]
	[on-double-click
	 (lambda (s e)
	   (cond
	    [(eq? do-on-double-click 'flip)
	     (begin-edit-sequence)
	     (let ([l (get-reverse-selected-list)])
	       (for-each 
		(lambda (s) 
		  (when (send s user-can-flip)
		    (send s flip)))
		l)
	       (let loop ([l (reverse l)])
		 (unless (null? l)
		   (set-before (car l) #f)
		   (loop (cdr l)))))
	     (no-selected)
	     (end-edit-sequence)]
	    [do-on-double-click
	     (do-on-double-click s)]
	    [else (void)]))])
      (public*
	[get-all-list
	 (lambda ()
	   (let loop ([t (find-first-snip)][accum null])
	     (cond
	      [(not t) (reverse accum)]
	      [else (loop (send t next) (cons t accum))])))]
	[get-full-box
	 (lambda ()
	   (let ([xb (box 0)][yb (box 0)]
			     [wb (box 0)][hb (box 0)])
	     (send (get-admin) get-view xb yb wb hb)
	     (values 0 0 (unbox wb) (unbox hb))))]
	[get-region-box
	 (lambda (region)
	   (values (region-x region)
		   (region-y region)
		   (region-w region)
		   (region-h region)))]
	[add-region
	 (lambda (r)
	   (set! regions (append regions (list r)))
	   (update-region r))]
	[remove-region
	 (lambda (r)
	   (set! regions (remq r regions))
	   (update-region r))]
	[unhilite-region
	 (lambda (region)
	   (set-region-hilite?! region #f)
	   (update-region region))]
	[hilite-region
	 (lambda (region)
	   (set-region-hilite?! region #t)
	   (update-region region))]
	[set-double-click-action
	 (lambda (a)
	   (set! do-on-double-click a))]
	[set-single-click-action
	 (lambda (a)
	   (set! do-on-single-click a))]
	[set-button-action
	 (lambda (button action)
	   (let ([map
		  (case action
		    [(drag/one) (list #t #f #f)]
		    [(drag-raise/one) (list #t #f #t)]
		    [(drag/above) (list #f #f #f)]
		    [(drag-raise/above) (list #f #f #t)]
		    [(drag/below) (list #f #t #f)]
		    [(drag-raise/below) (list #f #t #t)]
		    [else (error 'set-button-action "unknown action: ~s" action)])])
	     (set! button-map
		   (cons
		    (cons button map)
		    (remq (assoc button button-map)
			  button-map)))))])
      (super-make-object)
      (set-selection-visible #f)))

  (define table%
    (class mred:frame%
      (init title w h)
      (inherit reflow-container)
      (augment*
	[on-close
	 (lambda ()
	   (exit))])
      (public*
	[table-width (lambda ()
		       (reflow-container)
		       (let-values ([(x y w h) (send pb get-full-box)])
			 w))]
	[table-height (lambda ()
			(reflow-container)
			(let-values ([(x y w h) (send pb get-full-box)])
			  h))]
	[begin-card-sequence
	 (lambda ()
	   (set! in-sequence (add1 in-sequence))
	   (send pb begin-edit-sequence))]
	[end-card-sequence
	 (lambda ()
	   (send pb end-edit-sequence)
	   (set! in-sequence (sub1 in-sequence)))]
	[add-card
	 (lambda (card x y)
	   (position-cards (list card) x y (lambda (p) (values 0 0)) add-cards-callback))]
	[add-cards
	 (lambda (cards x y [offset (lambda (p) (values 0 0))])
	   (position-cards cards x y offset add-cards-callback))]
	[add-cards-to-region
	 (lambda (cards region)
	   (position-cards-in-region cards region add-cards-callback))]
	[move-card
	 (lambda (card x y)
	   (position-cards (list card) x y (lambda (p) (values 0 0)) move-cards-callback))]
	[move-cards
	 (lambda (cards x y  [offset (lambda (p) (values 0 0))])
	   (position-cards cards x y offset move-cards-callback))]
	[move-cards-to-region
	 (lambda (cards region)
	   (position-cards-in-region cards region (lambda (c x y) (send pb move-to c x y))))]
	[card-location
	 (lambda (card)
	   (let ([x (box 0)]
		 [y (box 0)])
	     (unless (send pb get-snip-location card x y)
	       (raise-mismatch-error 'card-location "card not on table: " card))
	     (values (unbox x) (unbox y))))]
	[all-cards
	 (lambda ()
	   (send pb get-all-list))]
	[remove-card
	 (lambda (card)
	   (remove-cards (list card)))]
	[remove-cards
	 (lambda (cards)
	   (begin-card-sequence)
	   (for-each (lambda (c) (send pb release-snip c)) cards)
	   (end-card-sequence))]
	[flip-card
	 (lambda (card)
	   (flip-cards (list card)))]
	[flip-cards
	 (lambda (cards)
	   (if (or (not animate?) (positive? in-sequence))
	       (for-each (lambda (c) (send c flip)) cards)
	       (let ([flip-step
		      (lambda (go)
			(let ([start (current-milliseconds)])
			  (begin-card-sequence)
			  (go)
			  (end-card-sequence)
			  (pause (max 0 (- (/ ANIMATION-TIME ANIMATION-STEPS)
					   (/ (- (current-milliseconds) start) 1000))))))])
		 (flip-step (lambda () (for-each (lambda (c) (send c semi-flip)) cards)))
		 (flip-step (lambda () (for-each (lambda (c) (send c flip)) cards)))
		 (flip-step (lambda () (for-each (lambda (c) (send c semi-flip)) cards))))))]
        [rotate-card
         (lambda (card mode) (rotate-cards (list card) mode))]
        [rotate-cards
         (lambda (cards mode)
           (begin-card-sequence)
           (let ([tw (table-width)]
                 [th (table-height)])
             (map (lambda (c)
                    (let ([w (send c card-width)]
                          [h (send c card-height)])
                      (send c rotate mode)
                      (let ([w2 (send c card-width)]
                            [h2 (send c card-height)]
                            [x (box 0)]
                            [y (box 0)])
                        (send pb get-snip-location c x y)
                        (send pb move-to c 
                              (min (max 0 (+ (unbox x) (/ (- w w2) 2))) (- tw w2))
                              (min (max 0 (+ (unbox y) (/ (- h h2) 2))) (- th h2))))))
                  cards)
             (end-card-sequence)))]
	[card-face-up
	 (lambda (card)
	   (cards-face-up (list card)))]
	[cards-face-up
	 (lambda (cards)
	   (flip-cards (filter (lambda (c) (send c face-down?)) cards)))]
	[card-face-down
	 (lambda (card)
	   (cards-face-down (list card)))]
	[cards-face-down
	 (lambda (cards)
	   (flip-cards (filter (lambda (c) (not (send c face-down?))) cards)))]
	[card-to-front
	 (lambda (card)
	   (send pb set-before card #f))]
	[card-to-back
	 (lambda (card)
	   (send pb set-after card #f))]
	[stack-cards
	 (lambda (cards)
	   (unless (null? cards)
	     (send pb only-front-selected) ; in case overlap changes
	     (begin-card-sequence)
	     (let loop ([l (cdr cards)][behind (car cards)])
	       (unless (null? l)
		 (send pb set-after (car l) behind)
		 (loop (cdr l) (car l))))
	     (end-card-sequence)))]
	[add-region
	 (lambda (r)
	   (send pb add-region r))]
	[remove-region
	 (lambda (r)
	   (send pb remove-region r))]
	[hilite-region
	 (lambda (r)
	   (send pb hilite-region r))]
	[unhilite-region
	 (lambda (r)
	   (send pb unhilite-region r))]
	[set-button-action
	 (lambda (button action)
	   (send pb set-button-action button action))]
	[set-double-click-action
	 (lambda (a)
	   (send pb set-double-click-action a))]
	[set-single-click-action
	 (lambda (a)
	   (send pb set-single-click-action a))]
	[pause
	 (lambda (duration)
	   (let ([s (make-semaphore)]
		 [a (alarm-evt (+ (current-inexact-milliseconds)
				  (* duration 1000)))]
		 [enabled? (send c is-enabled?)])
	     ;; Can't move the cards during this time:
	     (send c enable #f)
	     (mred:yield a)
	     (when enabled?
	       (send c enable #t))))]
	[animated
	 (case-lambda 
	  [() animate?]
	  [(on?) (set! animate? (and on? #t))])]
	[create-status-pane
	 (lambda ()
	   (let ([p (make-object mred:horizontal-pane% this)])
	     (set! msg (new mred:message% 
			    [parent p]
			    [label ""]
			    [stretchable-width #t]))
	     p))]
	[set-status
	 (lambda (str)
	   (when msg
	     (send msg set-label str)))]
	[add-help-button
	 (lambda (pane where title tt?)
	   (new mred:button% 
		(parent pane)
		(label (string-constant help-menu-label))
		(callback
		 (let ([show-help (show-help where title tt?)])
		   (lambda x
		     (show-help))))))]
	[add-scribble-button
	 (lambda (pane mod tag)
	   (new mred:button% 
		(parent pane)
		(label (string-constant help-menu-label))
		(callback
		 (let ([show-help (show-scribbling mod tag)])
		   (lambda x
		     (show-help))))))])
      (begin
	(define msg #f)
	(define add-cards-callback
	 (lambda (card x y)
	   (send pb insert card #f x y)))
        (define move-cards-callback
	 (lambda (card x y)
	   (send pb move-to card x y)
	   (send card remember-location pb))))
      (begin
	(define animate? #t)
	(define in-sequence 0))
      (private*
        [position-cards
	 (lambda (cards x y offset set)
	   (let ([positions (let loop ([l cards][n 0])
			      (if (null? l)
				  null
				  (let-values ([(dx dy) (offset n)])
				    (cons (cons (+ x dx) (+ y dy))
					  (loop (cdr l) (add1 n))))))])
	     (if (or (not animate?) (positive? in-sequence) (eq? set add-cards-callback))
		 (begin
		   (begin-card-sequence)
		   (for-each (lambda (c p) (set c (car p) (cdr p))) cards positions)
		   (end-card-sequence))
		 (let-values ([(moving-cards
				source-xs
				source-ys
				dest-xs
				dest-ys)
			       (let loop ([cl cards][pl positions])
				 (if (null? cl)
				     (values null null null null null)
				     (let-values ([(mcl sxl syl dxl dyl) (loop (cdr cl) (cdr pl))]
						  [(card) (car cl)]
						  [(x y) (values (caar pl) (cdar pl))])
				       (let ([xb (box 0)][yb (box 0)])
					 (send pb get-snip-location card xb yb)
					 (let ([sx (unbox xb)][sy (unbox yb)])
					   (if (and (= x sx) (= y sy))
					       (values mcl sxl syl dxl dyl)
					       (values (cons card mcl)
						       (cons sx sxl)
						       (cons sy syl)
						       (cons x dxl)
						       (cons y dyl))))))))])
		   (let ([time-scale
			  ;; An animation speed that looks good for
			  ;; long moves looks too slow for short
			  ;; moves. So scale the time back by as much
			  ;; as 50% if the max distance for all cards
			  ;; is short.
			  (let ([max-delta (max (apply max 0 (map (lambda (sx dx)
								    (abs (- sx dx)))
								  source-xs dest-xs))
						(apply max 0 (map (lambda (sy dy)
								    (abs (- sy dy)))
								  source-ys dest-ys)))])
			    (if (max-delta . < . 100)
				(/ (+ max-delta 100) 200.0)
				1))])
		     (let loop ([n 1])
		       (unless (> n ANIMATION-STEPS)
			 (let ([start (current-milliseconds)]
			       [scale (lambda (s d)
					(+ s (* (/ n ANIMATION-STEPS) (- d s))))])
			   (begin-card-sequence)
			   (for-each
			    (lambda (c sx sy dx dy)
			      (set c (scale sx dx) (scale sy dy)))
			    moving-cards
			    source-xs source-ys
			    dest-xs dest-ys)
			   (end-card-sequence)
			   (pause (max 0 (- (/ (* time-scale ANIMATION-TIME) ANIMATION-STEPS)
					    (/ (- (current-milliseconds) start) 1000))))
			   (loop (add1 n))))))))
	     ;; In case overlap changed:
	     (send pb only-front-selected)))]
	[position-cards-in-region
	 (lambda (cards r set)
           (unless (null? cards)
             (let-values ([(x y w h) (send pb get-region-box r)]
                          [(len) (sub1 (length cards))]
                          [(cw ch) (values (send (car cards) card-width)
                                           (send (car cards) card-height))])
               (let* ([pretty (lambda (cw) (+ (* (add1 len) cw) (* len PRETTY-CARD-SEP-AMOUNT)))]
                      [pw (pretty cw)]
                      [ph (pretty ch)])
                 (let-values ([(x w) (if (> w pw)
                                         (values (+ x (/ (- w pw) 2)) pw)
                                         (values x w))]
                              [(y h) (if (> h ph)
                                         (values (+ y (/ (- h ph) 2)) ph)
                                         (values y h))])
                   (position-cards cards x y
                                   (lambda (p)
                                     (if (zero? len)
                                         (values (/ (- w cw) 2)
                                                 (/ (- h ch) 2))
                                         (values (* (- len p) (/ (- w cw) len))
                                                 (* (- len p) (/ (- h ch) len)))))
                                   set))))))])
      (super-new [label title] [style '(metal no-resize-border)])
      (begin
        (define c (make-object mred:editor-canvas% this #f '(no-vscroll no-hscroll)))
	(define pb (make-object pasteboard%)))
      (send c min-client-width (+ 10 (inexact->exact (floor (* w (send back get-width))))))
      (send c min-client-height (+ 10 (inexact->exact (floor (* h (send back get-height))))))
      (send c stretchable-width #f)
      (send c stretchable-height #f)
      (send this stretchable-width #f)
      (send this stretchable-height #f)
      (send c set-editor pb)))

  (define (draw-roundish-rectangle dc x y w h)
    (send dc draw-line (+ x 1) y (+ x w -2) y)
    (send dc draw-line (+ x 1) (+ y h -1) (+ x w -2) (+ y h -1))
    (send dc draw-line x (+ y 1) x (+ y h -2))
    (send dc draw-line (+ x w -1) (+ y 1) (+ x w -1) (+ y h -2))))
