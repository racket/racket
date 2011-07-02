(module mrpopup mzscheme
  (require mzlib/class
           mzlib/class100
           mzlib/list
           (prefix wx: "kernel.rkt")
           (rename "wxme/cycle.rkt" wx:set-popup-menu%! set-popup-menu%!)
           "lock.rkt"
           "const.rkt"
           "helper.rkt"
           "check.rkt"
           "wx.rkt"
           "wxmenu.rkt"
           "mrmenuintf.rkt")

  (provide popup-menu%)

  (define popup-menu%
    (class100* mred% (menu-item-container<%> internal-menu<%>) ([title #f][popdown-callback void][demand-callback void][font no-val])
      (private-field 
       [callback demand-callback])
      (public
	[get-popup-target
	 (lambda ()
	   (send wx get-popup-grabber))]
	[get-items (entry-point (lambda () (send wx get-items)))]
	[on-demand (lambda ()
		     (callback this)
		     (for-each
		      (lambda (i) 
			(when (is-a? i labelled-menu-item<%>)
			  (send i on-demand)))
		      (send wx get-items)))]
	[set-min-width (lambda (n)
			 (check-range-integer '(method popup-menu% set-min-width) n)
			 (send wx set-width n))]
	[get-font (lambda ()
		    (send wx get-font))])
      (private-field
       [wx #f])
      (sequence
	(let ([cwho '(constructor popup-menu)])
	  (check-label-string/false cwho title)
	  (check-callback cwho popdown-callback)
	  (check-callback1 cwho demand-callback)
	  (check-font cwho font))
	(as-entry 
	 (lambda ()
	   (set! wx (make-object wx-menu% this title
				 (lambda (mwx e)
				   (let ([go
					  (lambda ()
					    (let ([wx (wx:id-to-menu-item (send e get-menu-id))])
					      (when wx
						(send (wx->mred wx) command (make-object wx:control-event% 'menu)))
					      (dynamic-wind
						  void
						  (lambda ()
						    (popdown-callback this (make-object wx:control-event% 
											(if wx 
											    'menu-popdown
											    'menu-popdown-none))))
						  (lambda () (send mwx popup-release)))))])
				     (if (eq? 'windows (system-type))
					 (wx:queue-callback go wx:middle-queue-key)
					 (go))))
				 (no-val->#f font)))
	   (super-init wx))))))

  (wx:set-popup-menu%! popup-menu%))
