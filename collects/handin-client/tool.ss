(module tool mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "unitsig.ss")
	   (lib "tool.ss" "drscheme")
	   (lib "etc.ss")
	   (lib "file.ss")
	   (lib "framework.ss" "framework")
	   (lib "sendurl.ss" "net")
           (lib "bitmap-label.ss" "mrlib")
	   "client.ss"
	   "info.ss"
	   ;; Temporary hack for test suite in separate window:
	   (lib "extension.ss" "test-suite"))

  (provide tool@)

  (define uninstalled? #f)

  (define server:port
    (#%info-lookup 'server:port (lambda () (getenv "PLT_HANDIN_SERVER_PORT"))))
  (define-values (server port-no)
    (if server:port
      (let ([m (regexp-match #rx"^(.+):([0-9]+)$" server:port)])
        (unless m
          (error 'handin-client
                 "Bad configuration ~s, expecting \"server:port\""
                 server:port))
        (values (cadr m) (string->number (caddr m))))
      (values #f #f)))

  (define handin-name (#%info-lookup 'name))
  (define this-collection (#%info-lookup 'collection))
  (define web-menu-name (#%info-lookup 'web-menu-name (lambda () #f)))
  (define web-address (#%info-lookup 'web-address (lambda () #f)))

  (preferences:set-default 'submit:username "" string?)
  (define (remembered-user)
    (preferences:get 'submit:username))
  (define (remember-user user)
    (preferences:set 'submit:username user))

  (define (connect)
    (handin-connect server
		    port-no
		    (build-path
		     (collection-path this-collection)
		     "server-cert.pem")))

  (define handin-frame%
    (class dialog%
      (inherit show is-shown?)
      (super-new [label "Handin"])

      (init-field content)

      (define status (new message%
			  [label (format "Making secure connection to ~a..." server)]
			  [parent this]
			  [stretchable-width #t]))
      (define username (new text-field%
			    [label "Username:"]
			    [init-value (remembered-user)]
			    [parent this]
			    [callback (lambda (t e) (activate-ok))]
			    [stretchable-width #t]))
      (define passwd (new text-field%
			  [label "Password:"]
			  [parent this]
			  [callback (lambda (t e) (activate-ok))]
			  [style '(single password)]
			  [stretchable-width #t]))
      (define assignment (new choice%
			      [label "Assignment:"]
			      [choices null]
			      [parent this]
			      [callback void]
			      [stretchable-width #t]))

      (define button-panel (new horizontal-pane%
				[parent this]
				[stretchable-height #f]))
      (make-object vertical-pane% button-panel) ; spacer
      (define ok (new button%
		      [label "Handin"]
		      [parent button-panel]
		      [callback (lambda (b e)
				  (disable-interface)
				  (send status set-label "Handing in...")
				  (parameterize ([current-custodian
						  comm-cust])
				    (thread
				     (lambda ()
				       (with-handlers ([void
							(lambda (exn)
							  (report-error
							   "Handin failed."
							   exn))])
					 (remember-user (send username get-value))
					 (submit-assignment
					  connection
					  (send username get-value)
					  (send passwd get-value)
					  (send assignment
						get-string
						(send assignment get-selection))
					  content
					  (lambda ()
					    (semaphore-wait commit-lock)
					    (send status set-label "Comitting...")
					    (set! committing? #t)
					    (semaphore-post commit-lock)))
					 (queue-callback
					  (lambda ()
					    (when abort-commit-dialog
					      (send abort-commit-dialog show #f))
					    (send status set-label "Handin successful.")
					    (set! committing? #f)
					    (done-interface))))))))]
		      [style '(border)]))

      (define ok-can-enable? #f)
      (define (activate-ok)
	(send ok enable (and ok-can-enable?
			     (not (string=? "" (send username get-value)))
			     (not (string=? "" (send passwd get-value))))))

      (define cancel  (new button%
			   [label "Cancel"]
			   [parent button-panel]
			   [callback (lambda (b e)
				       (let ([go? (begin
						    (semaphore-wait commit-lock)
						    (if committing?
							(begin
							  (semaphore-post commit-lock)
							  (send abort-commit-dialog show #t)
							  continue-abort?)
							#t))])
					 (when go?
					   (custodian-shutdown-all comm-cust)
					   (show #f))))]))

      (define continue-abort? #f)
      (define abort-commit-dialog
	(let ([d (make-object dialog% "Commit in Progress")])
	  (make-object message% "The commit action is in progress." d)
	  (make-object message% "Cancelling now may or may not work." d)
	  (make-object message% "Cancel anyway?" d)
	  (let ([b (new horizontal-panel%
			[parent d]
			[stretchable-height #f]
			[alignment '(center center)])])
	    (make-object button% "Continue Commit" d (lambda (b e) (send d show #f)))
	    (make-object button% "Try to Cancel" d (lambda (b e)
						     (set! continue-abort? #t)
						     (send d show #f))))))

      (define (disable-interface)
	(send ok enable #f)
	(send username enable #f)
	(send passwd enable #f)
	(send assignment enable #f))
      (define (enable-interface)
	(send ok enable #t)
	(send username enable #t)
	(send passwd enable #t)
	(send assignment enable #t)
        (send passwd focus))
      (define (done-interface)
        (send cancel set-label "Close")
        (send cancel focus))

      (define (report-error tag exn)
	(queue-callback
	 (lambda ()
	   (let* ([msg (if (exn? exn)
                         (let ([s (exn-message exn)])
                           (if (string? s)
                             s
                             (format "~e" s)))
                         (format "~e" exn))]
                  [retry? (regexp-match #rx"bad username or password for" msg)])
             (custodian-shutdown-all comm-cust)
             (disable-interface)
             (send status set-label tag)
             (when (is-shown?)
               (message-box "Server Error" msg this)
               (if retry?
                 (begin (init-comm) (semaphore-post go-sema) (enable-interface))
                 (done-interface)))))))

      (define go-sema #f)
      (define commit-lock #f)
      (define committing? #f)

      (define connection #f)

      (define comm-cust #f)
      (define (init-comm)
        (set! go-sema (make-semaphore 1))
        (set! commit-lock (make-semaphore 1))
        (set! comm-cust (make-custodian))
	(parameterize ([current-custodian comm-cust])
	  (thread (lambda ()
		    (let/ec escape
		      (with-handlers ([void
				       (lambda (exn)
					 (report-error
					  "Connection failed."
					  exn)
					 (escape))])
			(semaphore-wait go-sema)
			(let-values ([(h l) (connect)])
			  (when (null? l)
			    (error 'handin "there are no active assignments"))
			  (set! connection h)
			  (for-each (lambda (assign)
				      (send assignment append assign))
				    l)
			  (send assignment enable #t)
			  (set! ok-can-enable? #t)
			  (activate-ok)
			  (send status set-label (format "Connected securely for ~a." handin-name)))))))))

      (define/augment (on-close)
	(inner (void) on-close)
	(custodian-shutdown-all comm-cust))

      (send ok enable #f)
      (send assignment enable #f)

      (init-comm)
      (send passwd focus)
      (show #t)))

  (define (manage-handin-account)
    (new
     (class dialog%
       (inherit show is-shown?)
       (super-new [label "Handin Account"]
		  [alignment '(left center)])

       (define status (new message%
			   [label (format "Manage ~a account at ~a." handin-name server)]
			   [parent this]
			   [stretchable-width #t]))

       (define tabs (new tab-panel%
			 [parent this]
			 [choices '("Change Password"
				    "New User"
				    "Uninstall")]
			 [callback
			  (lambda (tp e)
			    (send single active-child
				  (list-ref 
				   (list old-user-box
					 new-user-box
					 uninstall-box)
				   (send tabs get-selection))))]))

       (define single (new panel:single%
			   [parent tabs]))

       (define (mk-txt label parent activate-ok)
	 (new text-field%
	      [label label]
	      [parent parent]
	      [callback (lambda (t e) (activate-ok))]
	      [stretchable-width #t]))

       (define (mk-passwd label parent activate-ok)
	 (new text-field%
	      [label label]
	      [parent parent]
	      [callback (lambda (t e) (activate-ok))]
	      [style '(single password)]
	      [stretchable-width #t]))

       (define (non-empty? t)
	 (not (string=? "" (send t get-value))))

       (define (activate-change)
	 (send change-button enable
	       (and (non-empty? old-username)
		    (non-empty? old-passwd)
		    (non-empty? new-passwd)
		    (non-empty? confirm-passwd))))
       (define old-user-box (new vertical-panel%
				 [parent single]
				 [alignment '(center center)]))
       (define old-username (mk-txt "Username:" old-user-box activate-change))
       (send old-username set-value (remembered-user))

       (define old-passwd (mk-passwd "Old:" old-user-box activate-change))
       (define new-passwd (mk-passwd "New:" old-user-box activate-change))
       (define confirm-passwd (mk-passwd "New again:" old-user-box activate-change))
       (define change-button (new button%
				  [label "Set Password"]
				  [parent old-user-box]
				  [callback
				   (lambda (b e)
				     (do-change/add #f old-username b e))]
				  [style '(border)]))

       (define (activate-new)
	 (send new-button enable
	       (and (non-empty? new-username)
		    (non-empty? full-name)
		    (non-empty? student-id)
		    (non-empty? add-passwd))))
       (define new-user-box (new vertical-panel%
				 [parent single]
				 [alignment '(center center)]))
       (define new-username (mk-txt "Username:" new-user-box activate-new))
       (send new-username set-value (remembered-user))
       (define full-name (mk-txt "Full Name:" new-user-box activate-new))
       (define student-id (mk-txt "ID:" new-user-box activate-new))
       (define add-passwd (mk-passwd "Password:" new-user-box activate-new))
       (define new-button (new button%
			       [label "Add User"]
			       [parent new-user-box]
			       [callback
				(lambda (b e)
				  (do-change/add #t new-username b e))]
			       [style '(border)]))

       (define uninstall-box (new vertical-panel%
				 [parent single]
				 [alignment '(center center)]))
       (define uninstall-button (new button%
				     [label (format "Uninstall ~a" handin-name)]
				     [parent uninstall-box]
				     [callback
				      (lambda (b e)
					(let ([dir (collection-path this-collection)])
					  (with-handlers ([void (lambda (exn)
								  (report-error
								   "Uninstall failed."
								   exn))])
					    (delete-directory/files dir)
					    (set! uninstalled? #t)
					    (send uninstall-button enable #f)
					    (message-box
					     "Uninstall"
					     (format
					      "The ~a tool has been uninstalled. ~a~a"
					      handin-name
					      "The Handin button and associated menu items"
					      " will not appear after you restart DrScheme.")))))]))
       (send uninstall-button enable (not uninstalled?))

       (define (report-error tag exn)
	 (queue-callback
	  (lambda ()
	    (custodian-shutdown-all comm-cust)
	    (send status set-label tag)
	    (when (is-shown?)
	      (message-box
	       "Server Error"
	       (if (exn? exn)
		   (let ([s (exn-message exn)])
		     (if (string? s)
			 s
			 (format "~e" s))))
	       this)
	      (set! comm-cust (make-custodian))))))

       (define comm-cust (make-custodian))
       (define/augment (on-close)
	 (inner (void) on-close)
	 (custodian-shutdown-all comm-cust))

       (define button-panel (new horizontal-pane%
				 [parent this]
				 [stretchable-height #f]))
       (make-object vertical-pane% button-panel) ; spacer
       (define cancel  (new button%
			    [label "Cancel"]
			    [parent button-panel]
			    [callback (lambda (b e)
					(custodian-shutdown-all comm-cust)
					(show #f))]))

       ;; Too-long fields can't damage the server, but they might
       ;;  result in confusing errors due to safety cut-offs on
       ;;  the server side.
       (define (check-length field size name k)
	 (when ((string-length (send field get-value)) . > . size)
	   (message-box "Error"
			(format "The ~a must be no longer than ~a characters."
				name size))
	   (k (void))))

       (define (do-change/add new? username b e)
	 (let/ec k
	   (unless new?
	     (check-length new-passwd 50 "New password" k)
	     (when (not (string=? (send new-passwd get-value)
				  (send confirm-passwd get-value)))
	       (message-box "Password Error"
			    "The \"New\" and \"New again\" passwords are not the same.")
	       (k (void))))
	   (when new?
	     (check-length username 50 "Username" k)
	     (check-length full-name 100 "Full Name" k)
	     (check-length student-id 100 "ID" k)
	     (check-length add-passwd 50 "Password" k))
	   (send tabs enable #f)
	   (parameterize ([current-custodian comm-cust])
	     (thread
	      (lambda ()
		(with-handlers ([void (lambda (exn)
                                        (send tabs enable #t)
					(report-error
					 "Update failed."
					 exn))])
		  (remember-user (send username get-value))
		  (send status set-label "Making secure connection...")
		  (let-values ([(h l) (connect)])
		    (send status set-label "Updating server...")
		    (if new?
			(submit-addition
			 h
			 (send username get-value)
			 (send full-name get-value)
			 (send student-id get-value)
			 (send add-passwd get-value))
			(submit-password-change
			 h
			 (send username get-value)
			 (send old-passwd get-value)
			 (send new-passwd get-value))))
		  (send status set-label "Success.")
		  (send cancel set-label "Close")))))))

       (send new-user-box show #f)
       (send uninstall-box show #f)
       (activate-new)
       (activate-change)
       (show #t))))

  (define (scale-by-half file)
    (let* ([bm (make-object bitmap% file)]
	   [w (send bm get-width)]
	   [h (send bm get-height)]
	   [bm2 (make-object bitmap% (quotient w 2) (quotient h 2))]
	   [mdc (make-object bitmap-dc% bm2)])
      (send mdc set-scale 0.5 0.5)
      (send mdc draw-bitmap bm 0 0)
      (send mdc set-bitmap #f)
      bm2))

  (define handin-icon
    (scale-by-half
     (build-path (collection-path this-collection) "icon.png")))

  (define (editors->string editors)
    (let* ([base (make-object editor-stream-out-bytes-base%)]
	   [stream (make-object editor-stream-out% base)])
      (write-editor-version stream base)
      (write-editor-global-header stream)
      (for-each (lambda (ed)
		  (send ed write-to-file stream)
		  (send ed write-to-file stream))
		editors)
      (write-editor-global-footer stream)
      (send base get-string)))

  (add-test-suite-extension
   "Handin"
   handin-icon
   (lambda (parent editor)
     (let ([content (editors->string (list editor))])
       (new handin-frame% [parent parent] [content content]))))

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      (define phase1 void)
      (define phase2 void)

      (define tool-button-label
	(bitmap-label-maker
	 "Handin"
	 handin-icon))

      (define (make-new-unit-frame% super%)
	(class super%
	  (inherit get-button-panel
		   get-definitions-text
		   get-interactions-text)
	  (super-instantiate ())

          (define/override (file-menu:between-open-and-revert file-menu)
            (new menu-item%
		 (label (format "Manage ~a..." handin-name))
		 (parent file-menu)
		 (callback (lambda (m e) (manage-handin-account))))
            (super file-menu:between-open-and-revert file-menu))

          (define/override (help-menu:after-about menu)
	    (when web-menu-name
	      (new menu-item%
		   (label web-menu-name)
		   (parent menu)
		   (callback (lambda (item evt)
			       (send-url web-address)))))
            (super help-menu:after-about menu))

	  (define button
	    (new button% 
		 [label (tool-button-label this)]
		 [parent (get-button-panel)]
		 [callback (lambda (button evt)
			     (let ([content (editors->string
					     (list (get-definitions-text)
						   (get-interactions-text)))])
			       (new handin-frame% [parent this] [content content])))]
		 [style '(deleted)]))
	  (send (get-button-panel) change-children
		(lambda (l) (cons button l)))))

      (when (and server port-no)
        (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f)))))
