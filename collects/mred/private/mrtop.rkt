(module mrtop mzscheme
  (require mzlib/class
	   mzlib/class100
	   mzlib/etc
	   mzlib/list
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "helper.ss"
	   "const.ss"
	   "kw.ss"
	   "check.ss"
	   "wx.ss"
	   "wxtop.ss"
	   "wxpanel.ss"
	   "mrwindow.ss"
	   "mrcontainer.ss")

  (provide top-level-window<%>
	   frame%
	   dialog%
	   (protect root-menu-frame
		    set-root-menu-frame!)
	   get-top-level-windows
	   get-top-level-focus-window
	   get-top-level-edit-target-window
	   send-message-to-window
	   (protect check-top-level-parent/false
		    check-frame-parent/false))

  (define top-level-window<%>
    (interface (area-container-window<%>)
      get-eventspace
      on-activate on-traverse-char on-system-menu-char
      can-close? on-close
      can-exit? on-exit
      get-focus-window get-edit-target-window
      get-focus-object get-edit-target-object
      center move resize
      on-message))

  (define-keywords top-level-window%-keywords
    window%-keywords container%-keywords area%-keywords)

  (define basic-top-level-window%
    (class100* (make-area-container-window% (make-window% #t (make-container% area%))) (top-level-window<%>) 
	       (mk-wx mismatches label parent)
      (inherit show)
      (rename [super-set-label set-label])
      (private
	[wx-object->proxy
	 (lambda (o)
	   (if (is-a? o wx:window%)
	       (wx->proxy o)
	       o))])
      (override
	[set-label (entry-point
		    (lambda (l)
		      (check-label-string/false '(method top-level-window<%> set-label) l)
		      (send wx set-title (or l ""))
		      (super-set-label l)))])
      (public
	[on-traverse-char (entry-point
			   (lambda (e)
			     (check-instance '(method top-level-window<%> on-traverse-char)
					     wx:key-event% 'key-event% #f e)
			     (send wx handle-traverse-key e)))]
	[on-system-menu-char (entry-point
			      (lambda (e)
				(check-instance '(method top-level-window<%> on-system-menu-char) 
						wx:key-event% 'key-event% #f e)
				(and (eq? #\space (send e get-key-code))
				     (send e get-meta-down)
				     (eq? 'windows (system-type))
				     (send wx system-menu) #t)))]
	[get-eventspace (entry-point (lambda () (send wx get-eventspace)))])
      (pubment
	[can-close? (lambda () (inner #t can-close?))]
	[on-close (lambda () (inner (void) on-close))])
      (public
	[can-exit? (lambda () (can-close?))]
	[on-exit (lambda () (on-close) (show #f))]
	[on-activate (lambda (x) (void))]
	[center (entry-point
		 (case-lambda
		  [() (send wx center 'both)]
		  [(dir) (send wx center dir)]))]
	[move (entry-point
	       (lambda (x y)
		 (check-slider-integer '(method top-level-window<%> move) x)
		 (check-slider-integer '(method top-level-window<%> move) y)
		 (send wx move x y)))]
	[resize (entry-point
		 (lambda (w h)
		   (check-range-integer '(method top-level-window<%> resize) w)
		   (check-range-integer '(method top-level-window<%> resize) h)
		   (send wx set-size -1 -1 w h)))]

	[get-focus-window (entry-point
			   (lambda () (let ([w (send wx get-focus-window)])
					(and w (wx->proxy w)))))]
	[get-edit-target-window (entry-point
				 (lambda () (let ([w (send wx get-edit-target-window)])
					      (and w (wx->proxy w)))))]
	[get-focus-object (entry-point
			   (lambda () (let ([o (send wx get-focus-object)])
					(and o (wx-object->proxy o)))))]
	[get-edit-target-object (entry-point
				 (lambda () (let ([o (send wx get-edit-target-object)])
					      (and o (wx-object->proxy o)))))]

	[on-message (lambda (m) (void))])
      (private-field
       [wx #f]
       [wx-panel #f]
       [finish (entry-point
		(lambda (top-level hide-panel?)
		  (set! wx-panel (make-object wx-vertical-panel% #f this top-level null #f))
		  (send (send wx-panel area-parent) add-child wx-panel)
		  (send top-level set-container wx-panel)
		  (when hide-panel?
		    (send wx-panel show #f))
		  top-level))])
      (sequence 
	(super-init (lambda () (set! wx (mk-wx finish)) wx) (lambda () wx-panel) mismatches label parent arrow-cursor))))


  (define frame%
    (class100*/kw basic-top-level-window% ()
		  [(label [parent #f] [width #f] [height #f] [x #f] [y #f] [style null])
		   top-level-window%-keywords]
      (inherit on-traverse-char on-system-menu-char)
      (sequence
	(let ([cwho '(constructor frame)])
	  (check-label-string cwho label)
	  (check-frame-parent/false cwho parent)
	  (check-dimension cwho width)
	  (check-dimension cwho height)
	  (check-init-pos-integer cwho x)
	  (check-init-pos-integer cwho y)
	  (check-style cwho #f '(no-resize-border no-caption no-system-menu 
						  mdi-parent mdi-child 
						  toolbar-button hide-menu-bar float metal) 
		       style)
	  (when (memq 'mdi-child style)
	    (when (memq 'mdi-parent style)
	      (raise-type-error (who->name cwho) 
				"style list, 'mdi-child and 'mdi-parent are mutually exclusive" 
				style)))))
      (rename [super-on-subwindow-char on-subwindow-char])
      (private-field
       [wx #f]
       [status-line? #f]
       [modified? #f])
      (override
	[on-subwindow-char (lambda (w event)
			     (super-on-subwindow-char w event)
			     (or (on-menu-char event)
				 (on-system-menu-char event)
				 (on-traverse-char event)))])
      (public
	[on-menu-char (entry-point
		       (lambda (e)
			 (check-instance '(method frame% on-menu-char) wx:key-event% 'key-event% #f e)
			 (send wx handle-menu-key e)))]
	[on-mdi-activate (lambda (on?) (void))]
	[on-toolbar-button-click (lambda () (void))]
	[create-status-line (entry-point (lambda () (unless status-line? (send wx create-status-line) (set! status-line? #t))))]
	[set-status-text (lambda (s) (send wx set-status-text s))]
	[has-status-line? (lambda () status-line?)]
	[iconize (entry-point (lambda (on?) (send wx iconize on?)))]
	[is-iconized? (entry-point (lambda () (send wx iconized?)))]
	[set-icon (case-lambda 
		   [(i) (send wx set-icon i)]
		   [(i b) (send wx set-icon i b)]
		   [(i b l?) (send wx set-icon i b l?)])]
	[maximize (entry-point (lambda (on?) (send wx position-for-initial-show) (send wx maximize on?)))]
        [is-maximized? (entry-point (lambda () (send wx is-maximized?)))]
	[get-menu-bar (entry-point (lambda () (let ([mb (send wx get-the-menu-bar)])
						(and mb (wx->mred mb)))))]
	[modified (entry-point
		   (case-lambda
		    [() modified?]
		    [(m) 
		     (set! modified? m)
		     (send wx set-modified m)]))])
      (sequence
	(as-entry
	 (lambda ()
	   (super-init 
	    (lambda (finish) 
	      (set! wx (finish (make-object wx-frame% this this
					    (and parent (mred->wx parent)) label
					    (or x -11111) (or y -11111)
					    (or width -1) (or height -1)
					    style)
			       (memq 'mdi-parent style)))
	      (send wx set-mdi-parent (memq 'mdi-parent style))
	      wx)
	    (lambda ()
	      (let ([cwho '(constructor frame)])
		(check-container-ready cwho parent)
		(when (memq 'mdi-child style)
		  (let ([pwx (and parent (mred->wx parent))])
		    (unless (and pwx (send pwx get-mdi-parent))
		      (raise-mismatch-error (who->name cwho) "parent for 'mdi-child frame is not an 'mdi-parent frame: " parent))))))
	    label parent))))))

  (define dialog%
    (class100*/kw basic-top-level-window% ()
		  [(label [parent #f] [width #f] [height #f] [x #f] [y #f] [style null])
		   top-level-window%-keywords]
      (inherit on-traverse-char on-system-menu-char center)
      (sequence
	(let ([cwho '(constructor dialog)])
	  (check-label-string cwho label)
	  (check-top-level-parent/false cwho parent)
	  (for-each (lambda (x) (check-dimension cwho x)) (list width height x y))
	  (check-style cwho #f '(no-caption resize-border no-sheet) style)))
      (rename [super-on-subwindow-char on-subwindow-char])
      (private-field [wx #f])
      (override
	[on-subwindow-char (lambda (w event)
			     (super-on-subwindow-char w event)
			     (or (on-system-menu-char event)
				 (on-traverse-char event)))])
      (sequence
	(as-entry
	 (lambda ()
	   (super-init (lambda (finish) 
			 (set! wx (finish (make-object wx-dialog% this this
						       (and parent (mred->wx parent)) label
						       (or x -11111) (or y -11111) (or width 0) (or height 0)
						       style)
					  #f))
			 wx)
		       (lambda () 
			 (let ([cwho '(constructor dialog)])
			   (check-container-ready cwho parent)))
		       label parent))))))

  (define root-menu-frame #f)
  (define (set-root-menu-frame! f) 
    (set! root-menu-frame f)
    (set-root-menu-wx-frame! (mred->wx f)))
  
  (define (get-top-level-windows)
    (remq root-menu-frame (map wx->mred (wx:get-top-level-windows))))

  (define (get-top-level-focus-window)
    (ormap (lambda (f) (and (send f is-act-on?) 
			    (let ([f (wx->mred f)])
			      (and f
				   (not (eq? f root-menu-frame))
				   f))))
	   (wx:get-top-level-windows)))

  (define (get-top-level-edit-target-window)
    (let loop ([l (wx:get-top-level-windows)][f #f][s 0][ms 0])
      (if (null? l)
	  f
	  (let* ([f2 (car l)]
		 [f2m (wx->mred f2)]
		 [s2 (send f2 get-act-date/seconds)]
		 [ms2 (send f2 get-act-date/milliseconds)])
	    (if (and (or (not f)
			 (> s2 s)
			 (and (= s2 s) (> ms2 ms)))
		     (not (eq? f2m root-menu-frame)))
		(loop (cdr l) f2m s2 ms2)
		(loop (cdr l) f s ms))))))

  (define (send-message-to-window x y m)
    (check-slider-integer 'send-message-to-window x)
    (check-slider-integer 'send-message-to-window y)
    (let ([w (wx:location->window x y)])
      (and w (let ([f (wx->proxy w)])
	       (and f 
		    (not (eq? f root-menu-frame))
		    (send f on-message m))))))

  (define (check-top-level-parent/false who p)
    (unless (or (not p) (is-a? p frame%) (is-a? p dialog%))
      (raise-type-error (who->name who) "frame% or dialog% object or #f" p)))

  (define (check-frame-parent/false who p)
    (unless (or (not p) (is-a? p frame%))
      (raise-type-error (who->name who) "frame% object or #f" p))))
