(module wxtextfield mzscheme
  (require mzlib/class
	   mzlib/class100
	   (prefix wx: "kernel.ss")
           (prefix wx: "wxme/text.ss")
           (prefix wx: "wxme/editor-canvas.ss")
	   "lock.ss"
	   "const.ss"
	   "check.ss"
	   "helper.ss"
	   "gdi.ss"
	   "wx.ss"
	   "wxwindow.ss"
	   "wxitem.ss"
	   "wxcanvas.ss"
	   "wxpanel.ss"
	   "editor.ss"
	   "mrpopup.ss")

  (provide (protect wx-text-field%))

  (define text-field-text% 
    (class100 text% (cb ret-cb control set-cb-mgrs!)
      (rename [super-on-char on-char])
      (inherit get-text last-position set-max-undo-history)
      (private-field
       [return-cb ret-cb])
      (private-field
       [block-callback 1]
       [callback
	(lambda (type)
	  (when (zero? block-callback)
	    (let ([e (make-object wx:control-event% type)])
	      (as-exit (lambda ()
			 (cb control e))))))])
      (override
	[on-char
	 (entry-point
	  (lambda (e)
	    (let ([c (send e get-key-code)])
	      (unless (and (or (eq? c #\return) (eq? c #\newline))
			   return-cb
			   (return-cb (lambda () (callback 'text-field-enter) #t)))
		(as-exit (lambda () (super-on-char e)))))))])
      (augment
       [after-insert
	(lambda args
	  (as-entry
	   (lambda ()
	     (callback 'text-field))))]
       [after-delete
	(lambda args
	  (as-entry
	   (lambda ()
	     (callback 'text-field))))])
      (sequence
	(set-cb-mgrs!
	 (lambda (thunk)
	   (dynamic-wind
	       (lambda () (set! block-callback (add1 block-callback)))
	       thunk
	       (lambda () (set! block-callback (sub1 block-callback)))))
	 (lambda () 
	   (set! block-callback 0)))
	(super-init)
	(set-max-undo-history 'forever))))
  
  (define wx-text-editor-canvas% 
    (class100* wx-editor-canvas% (wx-text-editor-canvas<%>) (mred proxy control parent style)
      (sequence
	(super-init mred proxy parent -1 -1 100 30 #f style 100 #f))))

  (define wx-text-field%
    (class100 wx-horizontal-panel% (mred proxy parent fun label value style _font)
      ;; Make text field first because we'll have to exit
      ;;  for keymap initializer
      (private-field
       [func fun]
       [font (or _font normal-control-font)]
       [without-callback #f]
       [callback-ready #f]
       [e (make-object text-field-text%
		       func
		       (lambda (do-cb)
			 (if multi?
			     #f
			     (do-cb)))
		       this
		       (lambda (wc cr)
			 (set! without-callback wc)
			 (set! callback-ready cr)))])
      (sequence
	(as-exit
	 (lambda ()
	   ((current-text-keymap-initializer) (send e get-keymap)))))
      (inherit alignment stretchable-in-y area-parent
	       get-min-size set-min-width set-min-height
               spacing)
      (rename [super-place-children place-children])
      (public
	[command (lambda (e)  ; No entry/exit needed
		   (check-instance '(method text-field% command) wx:control-event% 'control-event% #f e)
		   (func this e)
		   (void))]

	[get-editor (lambda () e)]
	
	[get-value (lambda () (send e get-text))] ; note: not as-entry when called
	[set-value (lambda (v) (without-callback
				(lambda () (send e insert v 0 (send e last-position)))))]

	[set-label (lambda (str) (when l (send l set-label str)))]
	[get-canvas-width (lambda ()
			    (let ([tw (box 0)])
			      (send c get-size tw (box 0))
			      (unbox tw)))]
        
        [set-field-background (lambda (col)
                                (send c set-canvas-background col))]
        [get-field-background (lambda ()
                                (send c get-canvas-background))])
      (override
	;; These might be called before we are fully initialized

	[set-cursor (lambda (c) (send e set-cursor c #t))]
	[set-focus (lambda () (when (object? c) (send c set-focus)))]
	
	[place-children
	 (lambda (children-info width height)
	   (if (null? children-info)
	       null
	       (let ([r (super-place-children children-info width height)])
		 (if horiz?
		     ;; Line up label right with text:
		     (cons (list* (caar r) (+ (cadar r) dy) (cddar r))
			   (cdr r))
		     r))))])
      (sequence
	(super-init #f proxy parent (if (memq 'deleted style) '(deleted) null) #f)
	(unless (memq 'deleted style)
	  (send (area-parent) add-child this)))
      (private-field
       [multi? (memq 'multiple style)]
       [horiz? (cond
		[(memq 'vertical-label style) #f]
		[(memq 'horizontal-label style) #t]
		[else (eq? (send (send parent get-window) get-label-position) 'horizontal)])]
       [dy 0]
       [p (if horiz?
	      this
	      (let ([p (make-object wx-vertical-pane% #f proxy this null #f)])
                (send p skip-subwindow-events? #t)
		(send (send p area-parent) add-child p)
		p))])
      (sequence
	(alignment 'left 'top)
	(unless horiz? (send p alignment 'left 'top))
	(unless multi? (stretchable-in-y #f))
	;; For Windows:
	(wx:set-combo-box-font font)
        (spacing 3))
      (private-field
       [l (and label
	       (make-object wx-message% #f proxy p label -1 -1 null font))]
       [c (make-object (class wx-text-editor-canvas% 
                         (define/override (on-combo-select i)
                           ((list-ref callbacks (- (length callbacks) i 1))))
                         (super-new))
                       #f proxy this p
		       (append
			'(control-border)
			(if (memq 'combo style)
			    '(combo)
			    null)
			(if multi?
			    (if (memq 'hscroll style)
				null
				'(hide-hscroll))
			    '(hide-vscroll hide-hscroll))))]
       [callbacks null])
      (public
        [append-combo-item (lambda (s cb)
                             (and (send c append-combo-item s)
                                  (set! callbacks (cons cb callbacks))
                                  #t))])
      (sequence
        (send c skip-subwindow-events? #t)
	(when l
          (send l skip-subwindow-events? #t)
	  (send l x-margin 0))
	(send c set-x-margin 2)
	(send c set-y-margin 2)
	(send e set-line-spacing 0)
	(send e set-paste-text-only #t)
	(send e auto-wrap (and multi? (not (memq 'hscroll style))))
	(let ([f font]
	      [s (send (send e get-style-list) find-named-style "Standard")])
	  (send s set-delta (let ([d (font->delta f)])
			      (if (memq 'password style)
				  (begin
				    (send d set-face #f)
				    (send d set-family 'modern)
				    (send d set-delta-foreground "darkgray")
				    (send d set-delta-background "darkgray"))
				  d))))
	(send c set-editor e)
	(send c set-line-count (if multi? 3 1))
	(unless multi? (send c set-single-line))

	(when (and l horiz?)
	  ;; Minimize vertical space around the label:
	  (send l y-margin 0)
	  ;; Find amount to drop label down to line up the baselines:
	  (let ([wbox (box 0)]
		[hbox (box 0)]
		[ybox (box 0)]
		[abox (box 0)])
	    ;; To bottom of first line
	    (send (send e get-admin) get-dc #f ybox)
	    (set! dy (+ (abs (unbox ybox)) (send e line-location 0 #f)))
	    
	    ;; Add diff for client size
	    (send c get-client-size wbox hbox)
	    (let ([d (- (send c get-height) (unbox hbox))])
	      (set! dy (+ dy (quotient d 2))))
	    
	    ;; Subtract descent of canvas-drawn text
	    (let ([font (send (send (send e get-style-list) find-named-style "Standard") get-font)])
	      (send c get-text-extent "hi" wbox hbox ybox #f font)
	      (set! dy (- dy (unbox ybox))))
	    
	    ;; Subtract ascent of label
	    (send l get-text-extent "hi" wbox hbox ybox abox font)
	    (set! dy (- dy (- (unbox hbox) (unbox ybox))))
	    
	    ;; Subtract space above label
	    (set! dy (- dy (quotient (- (send l get-height) (unbox hbox)) 2)))

	    ;; Exact
	    (set! dy (inexact->exact dy))))
	
	(when value
	  (set-value value)
	  (unless (string=? value "")
	    (let* ([ew (box 0)]
		   [cw (box 0)]
		   [tw (box 0)])
	      (send e get-extent ew #f)
	      (send (send e get-admin) get-view #f #f cw #f)
	      (send c get-size tw (box 0))
	      (let ([new-min-width (+ (unbox ew) (- (unbox tw) (unbox cw)))])
		(send c set-min-width (inexact->exact new-min-width))))))
	(let ([min-size (get-min-size)])
	  (set-min-width (car min-size))
	  (set-min-height (cadr min-size)))
	(callback-ready)))))
