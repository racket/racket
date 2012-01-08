(module mrwindow mzscheme
  (require mzlib/class
           mzlib/class100
           (prefix wx: "kernel.rkt")
           "lock.rkt"
           "helper.rkt"
           "const.rkt"
           "check.rkt"
           "wx.rkt"
           "kw.rkt"
           "wxwindow.rkt"
           "mrpopup.rkt")

  (provide area<%>
           (protect area%-keywords)
           area%
           (protect internal-subarea<%>)
           subarea<%>
           (protect subarea%-keywords
                    make-subarea%)
           window<%>
           (protect window%-keywords)
           subwindow<%>
           (protect make-window%)
           
           (protect set-get-outer-panel
                    set-parent))

  (define area<%>
    (interface ()
      get-parent get-top-level-window
      min-width min-height
      get-graphical-min-size
      stretchable-width stretchable-height))

  (define-keywords area%-keywords 
    [min-width no-val]
    [min-height no-val]
    [stretchable-width no-val]
    [stretchable-height no-val])

  (define-local-member-name
    set-get-outer-panel
    set-parent)

  (define area%
    (class100* mred% (area<%>) (mk-wx get-wx-pan get-outer-wx-pan mismatches prnt
				      ;; for keyword use:
				      [min-width no-val]
				      [min-height no-val]
				      [stretchable-width no-val]
				      [stretchable-height no-val])
      (sequence
	(let ([cwho '(iconstructor area)])
	  (unless (eq? min-width no-val) (check-non#f-dimension cwho min-width))
	  (unless (eq? min-height no-val) (check-non#f-dimension cwho min-height)))
	(mismatches))
      (private-field 
       [get-wx-outer-panel get-outer-wx-pan]
       [parent prnt])
      (public
        [set-parent (lambda (p) (set! parent p))] ; called in atomic mode
	[get-parent (lambda () parent)]
	[get-top-level-window (entry-point (lambda () (wx->mred (send wx get-top-level))))]
	[(minw min-width) (param get-wx-outer-panel min-width)]
	[(minh min-height) (param get-wx-outer-panel min-height)]
	[(sw stretchable-width) (param get-wx-outer-panel stretchable-in-x)]
	[(sh stretchable-height) (param get-wx-outer-panel stretchable-in-y)]
	[get-graphical-min-size (entry-point (lambda () 
					       (if (wx . is-a? . wx-basic-panel<%>)
						   (apply values (send wx get-graphical-min-size))
						   (send wx get-hard-minimum-size))))])
      (private-field
       [wx (mk-wx)])
      (sequence
	(super-init wx)
	(unless (eq? min-width no-val) (minw min-width))
	(unless (eq? min-height no-val) (minh min-height))
	(unless (eq? stretchable-width no-val) (sw stretchable-width))
	(unless (eq? stretchable-height no-val) (sh stretchable-height)))))

  (define internal-subarea<%> (interface ()))

  (define subarea<%> 
    (interface (area<%> internal-subarea<%>)
      horiz-margin vert-margin))

  (define-keywords subarea%-keywords
    [horiz-margin no-val]
    [vert-margin no-val])

  (define (make-subarea% %) ; % implements area<%>
    (class100* % (subarea<%>) (mk-wx get-wx-pan get-outer-wx-pan mismatches parent
				     ;; for keyword use
				     [horiz-margin no-val]
				     [vert-margin no-val])
      (sequence
	(let ([cwho '(iconstructor subarea)])
	  (unless (eq? horiz-margin no-val) (check-margin-integer cwho horiz-margin))
	  (unless (eq? vert-margin no-val) (check-margin-integer cwho vert-margin))))
      (private-field [get-wx-panel get-wx-pan])
      (public
	[(hm horiz-margin) (param get-wx-panel x-margin)]
	[(vm vert-margin) (param get-wx-panel y-margin)])
      (sequence 
	(super-init mk-wx get-wx-panel get-outer-wx-pan mismatches parent)
	(unless (eq? horiz-margin no-val) (hm horiz-margin))
	(unless (eq? vert-margin no-val) (vm vert-margin)))))

  (define window<%>
    (interface (area<%>)
      on-focus focus has-focus?
      on-size on-move
      accept-drop-files on-drop-file
      on-subwindow-char on-subwindow-event
      client->screen screen->client
      enable is-enabled? on-superwindow-enable
      get-label set-label get-plain-label
      get-client-size get-size get-width get-height get-x get-y
      get-cursor set-cursor popup-menu
      show is-shown? on-superwindow-show refresh
      get-handle get-client-handle))

  (define-keywords window%-keywords [enabled #t])

  (define subwindow<%> 
    (interface (window<%> subarea<%>)
      reparent))

  (define (make-window% top? %) ; % implements area<%>
    (class100* % (window<%>) (mk-wx get-wx-panel get-outer-wx-panel mismatches lbl parent crsr
				    ;; for keyword use
				    [enabled #t])
      (private-field [label lbl][cursor crsr])
      (public
	[popup-menu (entry-point 
		     (lambda (m x y)
		       (check-instance '(method window<%> popup-menu) popup-menu% 'popup-menu% #f m)
		       (check-range-integer '(method window<%> popup-menu) x)
		       (check-range-integer '(method window<%> popup-menu) y)
		       (let ([mwx (mred->wx m)])
			 (and (send mwx popup-grab this)
			      (as-exit
			       (lambda ()
				 (send m on-demand)
				 (send wx popup-menu mwx x y)))))))]
	[on-focus (lambda (x) (void))]
        [on-subwindow-focus (lambda (win active?) (void))]
	[on-size (lambda (w h)
		   (check-range-integer '(method window<%> on-size) w)
		   (check-range-integer '(method window<%> on-size) h))]
	[on-move (lambda (x y)
		   (check-slider-integer '(method window<%> on-move) x)
		   (check-slider-integer '(method window<%> on-move) y))]
	[on-subwindow-char (lambda (w e)
			     (check-instance '(method window<%> on-subwindow-char) window<%> 'window<%> #f w)
			     (check-instance '(method window<%> on-subwindow-char) wx:key-event% 'key-event% #f e)
			     #f)]
	[on-subwindow-event (lambda (w e)
			      (check-instance '(method window<%> on-subwindow-event) window<%> 'window<%> #f w)
			      (check-instance '(method window<%> on-subwindow-event) wx:mouse-event% 'mouse-event% #f e)
			      #f)]
	[on-drop-file (lambda (s)
			(unless (path-string? s)
			  (raise-type-error (who->name '(method window<%> on-drop-file)) "pathname string" s)))]

	[focus (entry-point (lambda () (send wx set-focus)))]
	[has-focus? (entry-point (lambda () (send wx has-focus?)))]
	[enable (entry-point (lambda (on?) (send wx enable on?)))]
	[is-enabled? (entry-point (lambda () (send wx is-enabled?)))]
	
	[get-label (lambda () label)]
	[set-label (lambda (l)
		     (check-label-string/false '(method window<%> set-label) l)
		     (set! label (if (string? l)
				     (string->immutable-string l)
				     l)))]
	[get-plain-label (lambda () (and (string? label) (wx:label->plain-label label)))]

	[get-handle (lambda () (send wx get-handle))]
	[get-client-handle (lambda () (send wx get-client-handle))]

	[accept-drop-files
	 (entry-point
	  (case-lambda
	   [() (send wx accept-drag?)]
	   [(on?) (send wx drag-accept-files on?)]))]
	
	[client->screen (entry-point
			 (lambda (x y)
			   (check-slider-integer '(method window<%> client->screen) x)
			   (check-slider-integer '(method window<%> client->screen) y)
			   (double-boxed
			    x y
			    (lambda (x y) (send wx client-to-screen x y)))))]
	[screen->client (entry-point
			 (lambda (x y)
			   (check-slider-integer '(method window<%> screen->client) x)
			   (check-slider-integer '(method window<%> screen->client) y)
			   (double-boxed
			    x y
			    (lambda (x y) (send wx screen-to-client x y)))))]
	[get-client-size (entry-point
			  (lambda ()
			    (double-boxed
			     0 0
			     (lambda (x y) (send wx get-client-size x y)))))]
	[get-size (entry-point
		   (lambda ()
		     (double-boxed
		      0 0
		      (lambda (x y) (send wx get-size x y)))))]
	
	[get-width (entry-point (lambda () (send wx get-width)))]
	[get-height (entry-point (lambda () (send wx get-height)))]
	[get-x (entry-point (lambda () (- (send wx get-x) (if top? 0 (send (send wx get-parent) ext-dx)))))]
	[get-y (entry-point (lambda () (- (send wx get-y) (if top? 0 (send (send wx get-parent) ext-dy)))))]
	
	[get-cursor (lambda () cursor)]
	[set-cursor (entry-point
		     (lambda (x)
		       (send wx set-cursor x)
		       (set! cursor x)))]

	[show (entry-point (lambda (on?) 
			     (when on?
			       (unless top?
				 (unless (memq wx (send (send wx area-parent) get-children))
				   (raise-mismatch-error 
				    (who->name '(method window<%> show))
				    "cannot show a subwindow that is not active in its parent: "
				    this))))
			     (send wx show on?)))]
	[is-shown? (entry-point (lambda () (send wx is-shown?)))]
	[on-superwindow-show (lambda (visible?) (void))]
	[on-superwindow-enable (lambda (active?) (void))]

	[refresh (entry-point (lambda () (send wx refresh)))])
      (private-field
       [wx #f])
      (sequence
	(super-init (lambda () (set! wx (mk-wx)) wx) get-wx-panel get-outer-wx-panel mismatches parent)
	(unless enabled (enable #f))))))
