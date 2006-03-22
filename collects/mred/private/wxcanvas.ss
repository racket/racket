(module wxcanvas mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   "wxkernel.ss"
	   "lock.ss"
	   "helper.ss"
	   "wx.ss"
	   "wxwindow.ss"
	   "wxitem.ss")

  (provide (protect make-canvas-glue%
		    wx-canvas%
		    wx-editor-canvas%))

  (define (make-canvas-glue% %) ; implies make-window-glue%
    (class100 (make-window-glue% %) (mred proxy . args)
      (inherit get-mred get-top-level)
      (public
	[do-on-char (lambda (e) (super on-char e))]
	[do-on-event (lambda (e) (super on-event e))]
	[do-on-scroll (lambda (e) (super on-scroll e))]
	[do-on-paint (lambda () (super on-paint))])
      (override
	[on-char (entry-point
		  (lambda (e)
		    (let ([mred (get-mred)])
		      (if mred
			  (as-exit (lambda () (send mred on-char e)))
			  (super on-char e)))))]
	[on-event (entry-point
		   (lambda (e)
		     (let ([mred (get-mred)])
		       (if mred
			   (as-exit (lambda () (send mred on-event e)))
			   (as-exit (lambda () (super on-event e)))))))]
	[on-scroll (entry-point
		    (lambda (e)
		      (let ([mred (get-mred)])
			(if mred
			    ;; Delay callback for Windows scrollbar 
			    ;; and Windows/Mac trampoiline
			    (queue-window-callback
			     this
			     (lambda () (send mred on-scroll e)))
			    (as-exit (lambda () (super on-scroll e)))))))]
	[on-paint (entry-point
		   (lambda ()
		     (let ([mred (get-mred)])
		       (if mred
			   (if (and (eq? 'windows (system-type))
				    (not (eq? (wx:current-eventspace)
					      (send (get-top-level) get-eventspace))))
			       ;; Windows circumvented the event queue; delay
			       (queue-window-callback
				this
				(lambda () (send mred on-paint)))
			       (as-exit (lambda () (send mred on-paint))))
			   (as-exit (lambda () (super on-paint)))))))])
      (sequence (apply super-init mred proxy args))))

  (define wx-canvas% 
    (make-canvas-glue%
     (class100 (make-control% wx:canvas% 0 0 #t #t) (parent x y w h style gl-config)
       (private-field
	[tabable? #f])
       (public
	 [on-tab-in (lambda () (send (wx->mred this) on-tab-in))]
	 [get-tab-focus (lambda () tabable?)]
	 [set-tab-focus (lambda (v) (set! tabable? v))])
       (override
	 [gets-focus? (lambda () tabable?)]
	 [handles-key-code
	  (lambda (code alpha? meta?)
	    (or meta? (not tabable?)))])
       (sequence
	 (super-init style parent x y w h style "canvas" gl-config)))))

  (define (make-editor-canvas% %)
    (class100 % (parent x y w h name style spp init-buffer)
      (inherit get-editor force-redraw
	       call-as-primary-owner min-height get-size
	       get-hard-minimum-size set-min-height)
      (private-field
       [fixed-height? #f]
       [fixed-height-lines 0]
       [orig-hard #f]
       [single-line-canvas? #f]
       [tabable? #f])
      (override
	[on-container-resize (lambda ()
			       (let ([edit (get-editor)])
				 (when edit
				   (as-exit (lambda () (send edit on-display-size-when-ready))))))]
	[on-scroll-on-change (lambda ()
			       (queue-window-callback
				this
				(lambda ()
				  (let ([edit (get-editor)])
				    (when edit
				      (send edit on-display-size-when-ready))))))]
	[on-set-focus
	 (entry-point
	  (lambda ()
	    (as-exit (lambda () (super on-set-focus)))
	    (let ([m (get-editor)])
	      (when m 
		(let ([mred (wx->mred this)])
		  (when mred
		    (as-exit (lambda () (send m set-active-canvas mred)))))))))]
	[set-editor
	 (letrec ([l (case-lambda
		      [(edit) (l edit #t)]
		      [(edit redraw?)
		       (let ([old-edit (get-editor)])
			 (super set-editor edit redraw?)
			 
			 (let ([mred (wx->mred this)])
			   (when mred
			     (when old-edit
			       (as-exit
				(lambda () (send old-edit remove-canvas mred))))
			     (when edit
			       (as-exit
				(lambda () (send edit add-canvas mred))))))

			 (update-size)
			 
			 ;; force-redraw causes on-container-resize to be called,
			 ;;  but only when the size of the canvas really matters
			 ;;  (i.e., when it is shown)
			 (force-redraw))])])
	   l)]
	[handles-key-code 
	 (lambda (x alpha? meta?)
	   (case x
	     [(#\tab #\return escape) (and (not tabable?)
					   (not single-line-canvas?))]
	     [else (not meta?)]))]


	[popup-for-editor (entry-point
			   (lambda (e m)
			     (let ([mwx (mred->wx m)])
			       (and (send mwx popup-grab e)
				    (as-exit (lambda () (send m on-demand) #t))
				    mwx))))])
      (public
	[set-tabable (lambda (on?) (set! tabable? on?))]
	[is-tabable? (lambda () tabable?)]
	[on-tab-in (lambda () 
		     (let ([mred (wx->mred this)])
		       (when mred
			 (send mred on-tab-in))))]
	[set-single-line (lambda () (set! single-line-canvas? #t))]
	[is-single-line? (lambda () single-line-canvas?)]
	[set-line-count (lambda (n)
			  (if n
			      (begin
				(unless orig-hard
				  (let-values ([(hmw hmh) (get-hard-minimum-size)])
				    (set! orig-hard hmh)))
				(set! fixed-height? #t)
				(set! fixed-height-lines n))
			      (when orig-hard
				(set! fixed-height? #f)
				(set-min-height orig-hard)))
			  (update-size))]
	[get-line-count (lambda () (and fixed-height? fixed-height-lines))]
	[update-size
	 (lambda ()
	   (let ([edit (get-editor)])
	     (when (and edit fixed-height?)
	       (let* ([top (if (is-a? edit wx:text%)
			       (send edit line-location 0 #t)
			       0)]
		      [bottom (if (is-a? edit wx:text%)
				  (send edit line-location 0 #f)
				  14)]
		      [height (- bottom top)])
		 (let* ([ch (box 0)]
			[h (box 0)])
		   (call-as-primary-owner
		    (lambda ()
		      (send (send edit get-admin) 
			    get-view #f #f #f ch)))
		   (get-size (box 0) h)
		   (let ([new-min-height (+ (* fixed-height-lines height) 
					    (- (unbox h) (unbox ch)))])
		     (set-min-height (inexact->exact (round new-min-height)))
		     (force-redraw)))))))])
      (override
	[set-y-margin (lambda (m)
			(super set-y-margin m)
			(when fixed-height? (update-size)))])
      
      (sequence
	(super-init style parent x y w h (or name "") style spp init-buffer)
	(when init-buffer
	  (let ([mred (wx->mred this)])
	    (when mred
	      (as-exit (lambda () (send init-buffer add-canvas mred)))))))))

  (define wx-editor-canvas% (make-canvas-glue%
			     (make-editor-canvas% (make-control% wx:editor-canvas%
								 0 0 #t #t)))))
