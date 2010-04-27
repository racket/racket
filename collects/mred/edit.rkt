
;; Simple editor implementation; provides new-text-frame
;; and new-pasteboard-frame

(module edit mzscheme
  (require mzlib/class
	   mred
	   mzlib/etc)

  (provide new-text-frame
	   new-pasteboard-frame
	   new-frame)

  (define (new-text-frame file) (new-frame text% file))
  (define (new-pasteboard-frame file) (new-frame pasteboard% file))

  (define new-frame
    (opt-lambda (editor% file [editor-canvas% editor-canvas%])
      (define f (make-object (class frame%
			       (inherit modified)
			       (define/augment (can-close?)
				 (and (or (not (modified))
					  (let ([r (message-box/custom
						    "Editor Modified"
						    "The editor has been modified. Really close it?"
						    "Close"
						    "Cancel"
						    "Save and Close"
						    this
						    '(default=2 disallow-close))])
					    (or (and (= r 3)
						     (send e save-file))
						(= r 1))))
				      (inner #t can-close?)))
			       (super-new))
			     "MrEdIt" #f 620 450))
      (define c (make-object editor-canvas% f))
      (define e (make-object (class editor%
                               (define/override (set-modified mod?)
				 (send f modified mod?)
				 (super set-modified mod?))
			       (super-new))))
      (define mb (make-object menu-bar% f))

      (define file-menu (make-object menu% "File" mb))
      (define edit-menu (make-object menu% "Edit" mb))
      (define font-menu (make-object menu% "Font" mb))
      (define para-menu (make-object menu% "Paragraph" mb))

      (make-object menu-item% "New Text Frame" file-menu
		   (lambda (item event) 
		     (new-text-frame #f))
		   #\N)
      (make-object menu-item% "New Pasteboard Frame" file-menu
		   (lambda (item event) 
		     (new-pasteboard-frame #f)))

      (make-object menu-item% "Open..." file-menu
		   (lambda (item event) 
		     (send e load-file ""))
		   #\O)
      (make-object menu-item% "Save As..." file-menu
		   (lambda (item event) 
		     (send e save-file ""))
		   #\S)
      (when (eq? editor% text%)
	(make-object menu-item% "Save As Text..." file-menu
		     (lambda (item event) 
		       (send e save-file "" 'text))))
      (make-object separator-menu-item% file-menu)
      (when (can-get-page-setup-from-user?)
        (make-object menu-item% "Page Setup..." file-menu
                     (lambda (item event)
                       (let ([s (get-page-setup-from-user #f f)])
                         (when s
                           (send (current-ps-setup) copy-from s))))
                     #\P
                     #f void
                     (cons 'shift (get-default-shortcut-prefix))))
      (make-object menu-item% "Print..." file-menu
		   (lambda (item event)
		     (send e print))
		   #\P)
      (make-object separator-menu-item% file-menu)
      (make-object menu-item% "Close" file-menu
		   (lambda (item event)
		     (when (send f can-close?)
		       (send f on-close)
		       (send f show #f)))
		   #\Q)

      (append-editor-operation-menu-items edit-menu #f)
      (when (eq? editor% text%)
	(make-object separator-menu-item% edit-menu)
	(make-object checkable-menu-item% "Wrap Lines" edit-menu
		     (lambda (item event)
		       (send e auto-wrap (send item is-checked?)))))

      (append-editor-font-menu-items font-menu)
      (let ([m (make-object menu% "Smoothing" font-menu)])
	(let ([mk (lambda (name v)
		    (make-object menu-item% name m
				 (lambda (i e)
				   (let* ([o (send f get-edit-target-object)])
				     (and o
					  (o . is-a? . editor<%>)
					  (send o change-style 
						(make-object style-delta% 'change-smoothing v)))))))])
	  (mk "Default" 'default)
	  (mk "Partly Smoothed" 'partly-smoothed)
	  (mk "Smoothed" 'smoothed)
	  (mk "Not Smoothed" 'unsmoothed)))

      (make-object menu-item% "Set Margins..." para-menu
		   (lambda (i ev)
		     (let* ([d (make-object dialog% "Margins" f)]
			    [mk-txt (lambda (label) (make-object 
						     text-field%
						     label
						     d
						     void
						     "0.0"))]
			    [first-left (mk-txt "First Left")]
			    [rest-left (mk-txt "Rest Left")]
			    [right (mk-txt "Right")]
			    [button-panel (new horizontal-pane% 
					       [parent d]
					       [alignment '(right center)])]
			    [ok (make-object button% "Ok" button-panel
					     (lambda (b ev)
					       (let* ([get (lambda (field)
							     (let ([n (string->number (send field get-value))])
							       (and n (real? n) (not (negative? n)) n)))]
						      [first-left (get first-left)]
						      [rest-left (get rest-left)]
						      [right (get right)])
						 (if (and first-left
							  rest-left
							  right)
						     (let ([start (send e position-paragraph
									(send e get-start-position))]
							   [end  (send e position-paragraph
								       (send e get-end-position))])
						       (let loop ([i start])
							 (unless (i . > . end)
							   (send e set-paragraph-margins 
								 i first-left rest-left right)
							   (loop (add1 i)))
							 (send d show #f)))
						     (bell))))
					     '(border))]
			    [cancel (make-object button% "Cancel" button-panel
						 (lambda (b e)
						   (send d show #f)))])
		       (send d show #t))))

      ((current-text-keymap-initializer) (send e get-keymap))
      (send c set-editor e)

      (when file
	(if (regexp-match "[.](gif|bmp|jpe?g|xbm|xpm|png)$" (string-downcase file))
	    (send e insert (make-object image-snip% file))
	    (send e load-file file)))

      (send e set-max-undo-history 'forever)
      
      (send f show #t)
      f)))
