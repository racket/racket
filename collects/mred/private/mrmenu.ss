(module mrmenu mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (lib "list.ss")
	   "wxkernel.ss"
	   "lock.ss"
	   "const.ss"
	   "helper.ss"
	   "check.ss"
	   "wx.ss"
	   "app.ss"
	   "wxmenu.ss"
	   "mrtop.ss"
	   "mrmenuintf.ss"
	   "mrpopup.ss")

  (provide separator-menu-item%
	   menu-item%
	   checkable-menu-item%
	   menu%
	   menu-bar%
	   (protect menu-parent-only
		    menu-or-bar-parent))

  ;; Most of the work is in the item. Anything that appears in a menubar or
  ;;  menu has an item. Submenus are created as instances of menu%, but
  ;;  menu% has a get-item method for manipulating the menu w.r.t. the parent
  ;;  (e.g., changing the title or enabled state). A popup menu, created
  ;;  as an instance of popup-menu%, has no item.
  ;;
  ;; A menu bar is created as a menu-bar%, given a frame as its parent. The
  ;;  frame must not already have a menu bar.
  ;;
  ;;  Plain labeled items are created as instances of menu-item% or
  ;;   checkable-menu-item%. The parent must be a menu-item-container<%>,
  ;;   which is a menu%, popup-menu%, or menu-bar%

  (define separator-menu-item%
    (class100* mred% (menu-item<%>) (parent)
      (sequence (menu-parent-only 'separator-menu-item parent))
      (private-field
       [prnt parent]
       [wx #f]
       [shown? #f]
       [wx-parent #f])
      (public
	[get-parent (lambda () prnt)]
	[restore (entry-point
		  (lambda ()
		    (unless shown?
		      (send wx-parent append-separator)
		      (send wx-parent append-item this wx)
		      (set! shown? #t))))]
	[delete (entry-point
		 (lambda ()
		   (when shown?
		     (send wx-parent delete-sep this wx)
		     (set! shown? #f))))]
	[is-deleted? (lambda () (not shown?))])
      (sequence
	(as-entry
	 (lambda ()
	   (set! wx (make-object wx-menu-item% this #f #f))
	   (set! wx-parent (send (mred->wx prnt) get-container))
	   (super-init wx)))
	(restore))))

  (define strip-tab 
    (if (menu-shortcut-in-label?)
	(lambda (s) 
	  (car (regexp-match #rx"^[^\t]*" s)))
	(lambda (s) 
	  (regexp-replace* #rx"&"
			   (regexp-replace* #rx"&(.)" 
					    (regexp-replace*
					     #rx" *[(]&.[)] *"
					     (car (regexp-match #rx"^[^\t]*" s))
					     "")
					    "\\1")
			   "\\&\\&"))))

  (define basic-labelled-menu-item%
    (class100* mred% (labelled-menu-item<%>) (prnt lbl help-str wx-sub chkble? keymap set-wx demand-callback)
      (private-field
       [parent prnt]
       [label lbl]
       [help-string help-str]
       [wx-submenu wx-sub]
       [checkable? chkble?]
       [callback demand-callback]
       [wx #f]
       [wx-parent #f]
       [plain-label (string->immutable-string (wx:label->plain-label label))]
       [in-menu? (is-a? parent internal-menu<%>)]
       [shown? #f]
       [enabled? #t])
      (private
	[do-enable (lambda (on?)
		     (when shown?
		       (if in-menu?
			   (send wx-parent enable wx (send wx id) on?)
			   (send wx-parent enable-top (send wx-parent position-of this) on?)))
		     (set! enabled? (and on? #t)))])
      (public
	[on-demand (lambda () (callback this))]
	[get-parent (lambda () parent)]
	[get-label (lambda () label)]
	[set-label (entry-point
		    (lambda (l)
		      (check-label-string '(method labelled-menu-item<%> set-label) l)
		      (set! label (string->immutable-string l))
		      (set-car! (send wx get-menu-data) l)  ; for meta-shortcuts
		      (set! plain-label (string->immutable-string (wx:label->plain-label l)))
		      (when shown?
			(if in-menu?
			    (send wx-parent set-label (send wx id) l)
			    (send wx-parent set-label-top (send wx-parent position-of this) label)))))]
	[get-plain-label (lambda () plain-label)]
	[get-help-string (lambda () help-string)]
	[set-help-string (entry-point
			  (lambda (s) 
			    (check-label-string/false '(method labelled-menu-item<%> set-help-string) s)
			    (set! help-string (and s (string->immutable-string s)))
			    (when in-menu?
			      (send wx-parent set-help-string (send wx id) help-string))))]
	[enable (lambda (on?) (do-enable on?))]
	[is-enabled? (lambda () enabled?)]
	[restore (entry-point
		  (lambda ()
		    (unless shown?
		      (if in-menu?
			  (begin
			    (if wx-submenu
				(send wx-parent append (send wx id) label wx-submenu help-string)
				(send wx-parent append (send wx id) label help-string checkable?))
			    (send wx-parent append-item this wx))
			  (send wx-parent append-item this wx-submenu (strip-tab label)))
		      (set! shown? #t)
		      (do-enable enabled?))))]
	[delete (entry-point
		 (lambda ()
		   (when shown?
		     (if in-menu?
			 (send wx-parent delete (send wx id) this)
			 (send wx-parent delete-item this))
		     (set! shown? #f))))]
	[is-deleted? (lambda () (not shown?))])
      (sequence
	(as-entry
	 (lambda ()
	   (when help-string
	     (set! help-string (string->immutable-string help-string)))
	   (set! wx (set-wx (make-object wx-menu-item% this (cons label #f) #t)))
	   (set! wx-parent (send (mred->wx parent) get-container))
	   (super-init wx)
	   (when keymap (send wx set-keymap keymap))))
	(restore))))

  (define (char-name c print?)
    (case c
      [(#\return) (if (eq? (system-type) 'macos) "Return" "Enter")]
      [(#\tab) "Tab"]
      [(#\space) "Space"]
      [(#\backspace) "Backspace"]
      [(#\rubout) "Delete"]
      [(#\:) (if print? ":" "Colon")]
      [(#\;) (if print? ";" "Semicolon")]
      [else c]))

  (define basic-selectable-menu-item%
    (class100* basic-labelled-menu-item% (selectable-menu-item<%>) (lbl checkable? mnu cb shrtcut help-string set-wx demand-callback)
      (inherit is-enabled?)
      (rename [super-restore restore] [super-set-label set-label]
	      [super-is-deleted? is-deleted?]
	      [super-is-enabled? is-enabled?]
	      [super-get-label get-label])
      (private-field
       [menu mnu]
       [callback cb]
       [label lbl]
       [shortcut shrtcut]
       [wx #f])
      (public
	[command (lambda (e)
		   (check-instance '(method selectable-menu-item<%> command) wx:control-event% 'control-event% #f e)
		   (void (callback this e)))])
      (private-field
       [x-prefix default-x-prefix])
      (private
	[calc-labels (lambda (label)
		       (let* ([new-label (if shortcut
					     (string-append
					      (strip-tab label)
					      (case (system-type)
						[(unix) (format "~a~a~a" #\tab 
								(case x-prefix
								  [(meta) "Meta+"]
								  [(alt) "Alt+"]
								  [(ctl-m) "Ctl+M "]
								  [(ctl) "Ctl+"])
								(char-name
								 (char-upcase shortcut)
								 #t))]
						[(windows) (format "~aCtl+~a" #\tab 
								   (char-name (char-upcase shortcut) #t))]
						[(macos macosx) (format "~aCmd+~a" #\tab 
									(char-name (char-upcase shortcut) #t))]))
					     (strip-tab label))]
			      [key-binding (and shortcut
						(case (system-type)
						  [(unix) (format "~a~a" 
								  (case x-prefix
								    [(meta) ":m:"]
								    [(alt) ":m:"]
								    [(ctl-m) ":c:m;:"]
								    [(ctl) ":c:"])
								  (char-name (char-downcase shortcut) #f))]
						  [(windows) (format ":c:~a" (char-name (char-downcase shortcut) #f))]
						  [(macos macosx) (format ":d:~a" (char-name (char-downcase shortcut) #f))]))]
			      [keymap (and key-binding
					   (let ([keymap (make-object wx:keymap%)])
					     (send keymap add-function "menu-item" 
						   ;; keymap function callback already in exit mode:
						   (lambda (edit event)
						     (if (is-enabled?)
							 (callback this (make-object wx:control-event% 'menu))
							 (wx:bell))))
					     (send keymap map-function key-binding "menu-item")
					     keymap))])
			 (values new-label keymap)))])
      (private
	[do-set-label (entry-point
		       (lambda (l)
			 (check-label-string '(method labelled-menu-item<%> set-label) l)
			 (let-values ([(new-label keymap) (calc-labels l)])
			   (set! label (string->immutable-string l))
			   (super-set-label new-label)
			   (if (super-is-deleted?)
			       (send wx set-keymap keymap)
			       (send wx swap-keymap menu keymap)))))])
      (override
	[get-label (lambda () label)]
	[set-label (lambda (s) (do-set-label s))])
      (public
	[set-shortcut (lambda (c) 
			(check-char/false '(method selectable-menu-item<%> set-shortcut) c)
			(unless (equal? shortcut c)
			  (set! shortcut c) 
			  (do-set-label label)))]
	[get-shortcut (lambda () shortcut)]
	[get-x-shortcut-prefix (lambda () x-prefix)]
	[set-x-shortcut-prefix (lambda (p) 
				 (unless (memq p '(meta alt ctl-m ctl))
				   (raise-type-error (who->name '(method selectable-menu-item<%> set-x-shortcut-prefix))
						     "symbol: meta, alt, ctl-m, or ctl" p))
				 (set! x-prefix p) (do-set-label label))])
      (sequence
	(set! label (string->immutable-string label))
	(let-values ([(new-label keymap) (calc-labels label)])
	  (super-init menu new-label help-string #f checkable? keymap (lambda (x) (set! wx x) (set-wx x)) demand-callback)))))

  (define (check-shortcut-args who label menu callback shortcut help-string demand-callback)
    (let ([cwho `(constructor ,who)])
      (check-label-string cwho label)
      (menu-parent-only who menu)
      (check-callback cwho callback)
      (check-char/false cwho shortcut)
      (check-label-string/false cwho help-string)
      (check-callback1 cwho demand-callback)))

  (define menu-item%
    (class100 basic-selectable-menu-item% (label parent callback [shortcut #f] [help-string #f] [demand-callback void])
      (sequence 
	(check-shortcut-args 'menu-item label parent callback shortcut help-string demand-callback)
	(super-init label #f parent callback shortcut help-string (lambda (x) x) demand-callback))))

  (define checkable-menu-item%
    (class100 basic-selectable-menu-item% (label parent callback [shortcut #f] [help-string #f] [demand-callback void] [checked #f])
      (sequence
	(check-shortcut-args 'checkable-menu-item label parent callback shortcut help-string demand-callback))
      (private-field
       [mnu parent]
       [wx #f])
      (public
	[check (entry-point (lambda (on?) (send (send (mred->wx mnu) get-container) check (send wx id) on?)))]
	[is-checked? (entry-point (lambda () (send (send (mred->wx mnu) get-container) checked? (send wx id))))])
      (sequence
	(super-init label #t mnu callback shortcut help-string (lambda (x) (set! wx x) x) demand-callback)
	(when checked (check #t)))))

  (define menu%
    (class100* basic-labelled-menu-item% (menu-item-container<%> internal-menu<%>) (label parent [help-string #f] [demand-callback void])
      (private-field 
       [callback demand-callback])
      (sequence 
	(check-label-string '(constructor menu) label)
	(menu-or-bar-parent 'menu parent)
	(check-label-string/false '(constructor menu) help-string)
	(check-callback1 '(constructor menu) demand-callback))
      (public
	[get-items (entry-point (lambda () (send wx-menu get-items)))])
      (override
	[on-demand (lambda ()
		     (callback this)
		     (for-each
		      (lambda (i) 
			(when (is-a? i labelled-menu-item<%>)
			  (send i on-demand)))
		      (send wx-menu get-items)))])
      (private-field
       [wx-menu #f])
      (sequence
	(as-entry
	 (lambda () 
	   (set! wx-menu (make-object wx-menu% this #f void #f))
	   (super-init parent label help-string wx-menu #f (send wx-menu get-keymap) (lambda (x) x) void)
	   (let ([wx-item (mred->wx this)])
	     (set-cdr! (send wx-item get-menu-data) wx-menu) ; for meta-shortcuts
	     (send wx-item set-wx-menu wx-menu)))))))

  (define menu-bar%
    (class100* mred% (menu-item-container<%>) (parent [demand-callback void])
      (sequence 
	(unless (or (is-a? parent frame%) (eq? parent 'root))
	  (raise-type-error (constructor-name 'menu-bar) "frame% object or 'root" parent))
	(check-callback1 '(constructor menu-bar) demand-callback)
	(if (eq? parent 'root)
	    (unless (current-eventspace-has-menu-root?)
	      (raise-mismatch-error (constructor-name 'menu-bar) "no menu bar allowed in the current eventspace for: " parent))
	    (when (as-entry (lambda () (send (mred->wx parent) get-the-menu-bar)))
	      (raise-mismatch-error (constructor-name 'menu-bar) "the specified frame already has a menu bar: " parent))))
      (private-field 
       [callback demand-callback]
       [prnt (if (eq? parent 'root)
		 (let ([f (make-object (class frame%
					 (define/override (on-exit)
					   (exit))
					 (super-make-object "Root")))])
		   (as-entry
		    (lambda ()
		      (when root-menu-frame
			(raise-mismatch-error (constructor-name 'menu-bar) "already has a menu bar: " parent))
		      (send (mred->wx f) designate-root-frame)
		      (set-root-menu-frame! f)))
		   f)
		 parent)]
       [wx #f]
       [wx-parent #f]
       [shown? #f])
      (public
	[get-frame (lambda () prnt)]
	[get-items (entry-point (lambda () (send wx get-items)))]
	[enable (entry-point (lambda (on?) (send wx enable-all on?)))]
	[is-enabled? (entry-point (lambda () (send wx all-enabled?)))]
	[on-demand (lambda ()
		     (callback this)
		     (for-each
		      (lambda (i) (send i on-demand))
		      (send wx get-items)))])
      (sequence
	(as-entry
	 (lambda ()
	   (set! wx (make-object wx-menu-bar% this))
	   (set! wx-parent (mred->wx prnt))
	   (super-init wx)
	   (send wx-parent set-menu-bar wx)
	   (send wx-parent self-redraw-request))))))

  (define (menu-parent-only who p)
    (unless (is-a? p internal-menu<%>)
      (raise-type-error (constructor-name who) "parent menu% or popup-menu% object" p)))

  (define (menu-or-bar-parent who p)
    (unless (or (is-a? p internal-menu<%>) (is-a? p menu-bar%))
      (raise-type-error (constructor-name who) "built-in menu-item-container<%> object" p)))
  
  (wx:set-menu-tester (lambda (m) (is-a? m popup-menu%))))
