(module mrtextfield mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (lib "list.ss")
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "const.ss"
	   "check.ss"
	   "helper.ss"
	   "wx.ss"
	   "kw.ss"
	   "wxtextfield.ss"
	   "mrcontainer.ss"
	   "mritem.ss"
	   "mrmenu.ss"
	   "mrpopup.ss")

  (provide text-field%
	   combo-field%)

  (define combo-flag (gensym))

  (define (check-text-field-args cwho
				 label
				 choices? choices
				 parent
				 callback
				 init-value
				 style req-styles
				 font)
    (check-label-string/false cwho label)
    (when choices?
      (unless (and (list? choices) (andmap label-string? choices))
	(raise-type-error (who->name cwho) "list of strings (up to 200 characters)" choices)))
    (check-container-parent cwho parent)
    (check-callback cwho callback)
    (check-string cwho init-value)
    (check-style cwho 
		 req-styles 
		 (append
		  (if choices? null '(hscroll password))
		  '(vertical-label horizontal-label deleted))
		 (remq combo-flag style))
    (check-font cwho font))

  (define text-field%
    (class100*/kw basic-control% () 
		  [(label parent [callback (lambda (b e) (void))] [init-value ""] [style '(single)])
		   control%-keywords]
      (sequence 
	(check-text-field-args '(constructor text-field)
			       label 
			       #f #f
			       parent callback init-value
			       style '(single multiple)
			       font))
      (private-field
       [wx #f])
      (public
	[get-editor (entry-point (lambda () (send wx get-editor)))]
	[get-value (lambda () (send wx get-value))] ; note: wx method doesn't expect as-entry
	[set-value (entry-point 
		    (lambda (v) 
		      (check-string '(method text-control<%> set-value) v)
		      (send wx set-value v)))])
      (sequence
	;; Technically a bad way to change margin defaults, since it's
	;;  implemented with an update after creation:
	(when (eq? horiz-margin no-val) (set! horiz-margin 2))
	(when (eq? vert-margin no-val) (set! vert-margin 2))
	(as-entry
	 (lambda ()
	   (super-init (lambda () 
			 (set! wx (make-object wx-text-field% this this
					       (mred->wx-container parent) (wrap-callback callback)
					       label init-value 
					       (if (memq combo-flag style)
						   (cons 'combo (remq combo-flag style))
						   style)
					       (no-val->#f font)))
			 wx)
		       (lambda ()
			 (let ([cwho '(constructor text-field)])
			   (check-container-ready cwho parent)))
		       label parent callback ibeam))))))

  (define combo-field%
    (class100*/kw text-field% () 
		  [(label choices parent [callback (lambda (b e) (void))] [init-value ""] [style '()])
		   control%-keywords]
      (inherit set-value popup-menu get-size focus get-editor)
      (sequence 
	(check-text-field-args '(constructor combo-field)
			       label 
			       #f choices
			       parent callback init-value
			       style #f 
			       font))
      (public
	[on-popup (lambda (e)
		    (let-values ([(w h) (get-size)]
				 [(cw) (send (mred->wx this) get-canvas-width)])
		      (send menu set-min-width cw)
		      (popup-menu menu (- w cw) h)))]
	[get-menu (lambda () menu)]
	[append (lambda (item)
		  (check-label-string '(method combo-field% append) item)
		  (make-object menu-item% item menu 
			       (lambda (i e)
				 (focus)
				 (set-value item)
				 (let ([e (get-editor)])
				   (send e set-position 0 (send e last-position))))))])
      (override
	[on-subwindow-event (lambda (w e)
			      (and (send e button-down?)
				   (let-values ([(cw) (send (mred->wx this) get-canvas-width)])
				     (and ((send e get-x) . >= . (- cw side-combo-width))
					  (begin
					    (on-popup e)
					    #t)))))])
      (private-field
       [menu (new popup-menu% [font font])])
      (sequence
	(for-each (lambda (item) 
		    (append item))
		  choices)
	(super-init label parent callback init-value (list* combo-flag 'single style))))))
