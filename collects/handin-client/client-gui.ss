(module client-gui mzscheme
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

  (define handin-dialog-name (string-append handin-name " Handin"))
  (define button-label/h     (string-append handin-name " Handin"))
  (define button-label/r     (string-append handin-name " Retrieve"))
  (define manage-dialog-name (string-append handin-name " Handin Account"))

  (define preference-key
    (string->symbol (format "submit:username:~a" this-collection)))

  (preferences:set-default preference-key "" string?)
  (define (remembered-user)
    (preferences:get preference-key))
  (define (remember-user user)
    (preferences:set preference-key user))

  (define (connect)
    (handin-connect
     server port-no
     (build-path (collection-path this-collection) "server-cert.pem")))

  (define handin-frame%
    (class dialog%
      (inherit show is-shown? center)
      (super-new [label handin-dialog-name])

      (init-field content open-drscheme-window)

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

      (define retrieve?
        (new check-box%
             [label "Retrieve"]
             [parent button-panel]
             [callback (lambda _
                         (define r? (send retrieve? get-value))
                         (send ok set-label
                               (if r? button-label/r button-label/h)))]))

      (define (submit-file)
        (define final-message "Handin successful.")
        (submit-assignment
         connection
         (send username get-value)
         (send passwd get-value)
         (send assignment get-string (send assignment get-selection))
         content
         ;; on-commit
         (lambda ()
           (semaphore-wait commit-lock)
           (send status set-label "Committing...")
           (set! committing? #t)
           (semaphore-post commit-lock))
         ;; message/message-final/message-box handlers
         (lambda (msg) (send status set-label msg))
         (lambda (msg) (set! final-message msg))
         (lambda (msg styles) (message-box "Handin" msg this styles)))
        (queue-callback
         (lambda ()
           (when abort-commit-dialog (send abort-commit-dialog show #f))
           (send status set-label final-message)
           (set! committing? #f)
           (done-interface))))
      (define (retrieve-file)
        (let ([buf (retrieve-assignment
                    connection
                    (send username get-value)
                    (send passwd get-value)
                    (send assignment get-string (send assignment get-selection)))])
          (queue-callback
           (lambda ()
             (done-interface)
             (do-cancel-button)
             (string->editor! buf (send (open-drscheme-window) get-editor))))))

      (define ok
        (new button%
          [label ; can change to button-label/r, so use extra spaces
           (string-append " " button-label/h " ")]
          [parent button-panel]
          [style '(border)]
          [callback
           (lambda (b e)
             (disable-interface)
             (send status set-label "Handing in...")
             (parameterize ([current-custodian comm-cust])
               (thread
                (lambda ()
                  (remember-user (send username get-value))
                  (with-handlers ([void (lambda (exn)
                                          (report-error "Handin failed." exn))])
                    (if (send retrieve? get-value)
                      (retrieve-file)
                      (submit-file)))))))]))

      (define ok-can-enable? #f)
      (define (activate-ok)
	(send ok enable (and ok-can-enable?
			     (not (string=? "" (send username get-value)))
			     (not (string=? "" (send passwd get-value))))))

      (define cancel  (new button%
			   [label "Cancel"]
			   [parent button-panel]
			   [callback (lambda (b e) (do-cancel-button))]))
      (define (do-cancel-button)
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
	    (show #f))))

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

      (define interface-widgets
        (list ok username passwd assignment retrieve?))
      (define (disable-interface)
        (for-each (lambda (x) (send x enable #f)) interface-widgets))
      (define (enable-interface)
        (for-each (lambda (x) (send x enable #t)) interface-widgets))
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
	     (set! committing? #f)
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
	  (thread
           (lambda ()
             (let/ec escape
               (with-handlers ([void
                                (lambda (exn)
                                  (report-error "Connection failed." exn)
                                  (escape))])
                 (semaphore-wait go-sema)
                 (let* ([h (connect)]
                        [l (retrieve-active-assignments h)])
                   (when (null? l)
                     (handin-disconnect h)
                     (error 'handin "there are no active assignments"))
                   (set! connection h)
                   (for-each (lambda (assign) (send assignment append assign))
                             l)
                   (send assignment enable #t)
                   (set! ok-can-enable? #t)
                   (activate-ok)
                   (send status set-label
                         (format "Connected securely for ~a." handin-name)))))))))

      (define/augment (on-close)
	(inner (void) on-close)
	(do-cancel-button))

      (send ok enable #f)
      (send assignment enable #f)

      (init-comm)
      (send passwd focus)
      (center)
      (show #t)))

  (define (manage-handin-account)
    (new
     (class dialog%
       (inherit show is-shown? center)
       (super-new [label manage-dialog-name]
		  [alignment '(left center)])

       (define EXTRA-FIELDS
         (let ([ef #f])
           (lambda ()
             (unless ef (set! ef (retrieve-extra-fields (connect))))
             ef)))

       (define status
         (new message%
              [label (format "Manage ~a handin account at ~a." handin-name server)]
              [parent this]
              [stretchable-width #t]))

       (define tabs
         (new tab-panel%
              [parent this]
              [choices '("New User" "Change Info" "Uninstall")]
              [callback
               (lambda (tp e)
                 (send single active-child
                       (list-ref (list new-user-box old-user-box uninstall-box)
                                 (send tabs get-selection))))]))

       (define single (new panel:single% [parent tabs]))

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

       (define (non-empty? . ts)
	 (andmap (lambda (t) (not (string=? "" (send t get-value)))) ts))

       (define (same-value t1 t2)
         (string=? (send t1 get-value) (send t2 get-value)))

       (define (activate-change)
         (define an-extra-non-empty? (ormap non-empty? change-extra-fields))
	 (send retrieve-old-info-button enable
               (non-empty? old-username old-passwd))
	 (send change-button enable
               (and (same-value new-passwd new-passwd2)
                    (non-empty? old-username old-passwd)
                    (or (non-empty? new-passwd) an-extra-non-empty?)))
         (send change-button set-label
               (if an-extra-non-empty? "Change Info" "Set Password")))

       (define old-user-box (new vertical-panel%
				 [parent single]
				 [alignment '(center center)]))
       (define old-username (mk-txt "Username:" old-user-box activate-change))
       (send old-username set-value (remembered-user))

       (define old-passwd
         (mk-passwd "Old Password:" old-user-box activate-change))
       (define change-extra-fields
         (map (lambda (f)
                (mk-txt (format "~a:" (car f)) old-user-box activate-change))
              (EXTRA-FIELDS)))
       (define new-passwd
         (mk-passwd "New Password:" old-user-box activate-change))
       (define new-passwd2
         (mk-passwd "New Password again:" old-user-box activate-change))

       (define-values (retrieve-old-info-button change-button)
         (let ([p (new horizontal-pane%
                       [parent old-user-box]
                       [stretchable-height #f]
                       [alignment '(center center)])])
           (make-object vertical-pane% p)
           (values
            (begin0 (new button% [label "Get Current Info"] [parent p]
                         [callback (lambda (b e) (do-retrieve old-username))])
              (make-object vertical-pane% p))
            (begin0 (new button% [label "Set Password"] [parent p] [style '(border)]
                         [callback (lambda (b e)
                                     (do-change/add #f old-username))])
              (make-object vertical-pane% p)))))

       (define (activate-new)
	 (send new-button enable
	       (and (apply non-empty? new-username add-passwd add-passwd2
                           add-extra-fields)
                    (same-value add-passwd add-passwd2))))
       (define new-user-box (new vertical-panel%
				 [parent single]
				 [alignment '(center center)]))
       (define new-username (mk-txt "Username:" new-user-box activate-new))
       (send new-username set-value (remembered-user))
       (define add-extra-fields
         (map (lambda (f)
                (mk-txt (format "~a:" (car f)) new-user-box activate-new))
              (EXTRA-FIELDS)))
       ;; (define full-name  (mk-txt "Full Name:" new-user-box activate-new))
       ;; (define student-id (mk-txt "ID:" new-user-box activate-new))
       ;; (define email      (mk-txt "Email:" new-user-box activate-new))
       (define add-passwd  (mk-passwd "Password:" new-user-box activate-new))
       (define add-passwd2 (mk-passwd "Password again:" new-user-box activate-new))
       (define new-button (new button%
			       [label "Add User"]
			       [parent new-user-box]
			       [callback (lambda (b e)
                                           (do-change/add #t new-username))]
			       [style '(border)]))

       (define uninstall-box (new vertical-panel%
				 [parent single]
				 [alignment '(center center)]))
       (define uninstall-button (new button%
				     [label (format "Uninstall ~a Handin" handin-name)]
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

       (define (do-change/add new? username)
	 (let/ec k
           (check-length username 50 "Username" k)
           (let* ([pw1 (if new? new-passwd  add-passwd)]
                  [pw2 (if new? new-passwd2 add-passwd2)]
                  [l1 (regexp-replace #rx" *:$" (send pw1 get-label) "")]
                  [l2 (regexp-replace #rx" *:$" (send pw2 get-label) "")])
             (check-length pw1 50 l1 k)
             ;; not really needed, but leave just in case
             (unless (string=? (send pw1 get-value) (send pw2 get-value))
               (message-box "Password Error"
                 (format "The \"~a\" and \"~a\" passwords are not the same."
                         l1 l2))
               (k (void))))
           (for-each (lambda (t f) (check-length t 100 (car f) k))
                     (if new? add-extra-fields change-extra-fields)
                     (EXTRA-FIELDS))
	   (send tabs enable #f)
	   (parameterize ([current-custodian comm-cust])
	     (thread
	      (lambda ()
		(with-handlers
                    ([void (lambda (exn)
                             (send tabs enable #t)
                             (report-error
                              (format "~a failed." (if new? "Creation" "Update"))
                              exn))])
		  (remember-user (send username get-value))
		  (send status set-label "Making secure connection...")
		  (let ([h (connect)])
                    (define (run proc . fields)
                      (apply proc h
                             (let loop ([x fields])
                               (if (list? x) (map loop x) (send x get-value)))))
		    (send status set-label
                          (if new? "Creating user..." "Updating server..."))
		    (if new?
			(run submit-addition username add-passwd
                             add-extra-fields)
			(run submit-info-change username old-passwd new-passwd
                             change-extra-fields)))
		  (send status set-label "Success.")
		  (send cancel set-label "Close")))))))

       (define (do-retrieve username)
         (let/ec k
	   (send tabs enable #f)
	   (parameterize ([current-custodian comm-cust])
	     (thread
	      (lambda ()
		(with-handlers ([void (lambda (exn)
                                        (send tabs enable #t)
					(report-error "Retrieve failed." exn))])
		  (remember-user (send username get-value))
		  (send status set-label "Making secure connection...")
		  (let ([h (connect)])
                    (define (run proc . fields)
                      (apply proc h
                             (let loop ([x fields])
                               (if (list? x) (map loop x) (send x get-value)))))
		    (send status set-label "Retrieving information...")
                    (let ([vals (run retrieve-user-info username old-passwd)])
                      (send status set-label
                            "Success, you can now edit fields.")
                      (send tabs enable #t)
                      (for-each (lambda (f val) (send f set-value val))
                                change-extra-fields vals)
                      (activate-change)))))))))

       (send new-user-box show #f)
       (send old-user-box show #f)
       (send uninstall-box show #f)
       (let ([new? (equal? "" (remembered-user))])
         (send (if new? new-user-box old-user-box) show #t)
         (send tabs set-selection (if new? 0 1)))
       (activate-new)
       (activate-change)
       (center)
       (show #t))))

  (define (scale-by-half file)
    (let* ([bm (make-object bitmap% file 'unknown/mask)]
	   [w (send bm get-width)]
	   [h (send bm get-height)]
	   [bm2 (make-object bitmap% (quotient w 2) (quotient h 2))]
	   [mbm2 (and (send bm get-loaded-mask)
		      (make-object bitmap% (quotient w 2) (quotient h 2)))]
	   [mdc (make-object bitmap-dc% bm2)])
      (send mdc draw-bitmap-section-smooth bm 
	    0 0 (quotient w 2) (quotient h 2)
	    0 0 w h)
      (send mdc set-bitmap #f)
      (when mbm2
	(send mdc set-bitmap mbm2)
	(send mdc draw-bitmap-section-smooth (send bm get-loaded-mask)
	      0 0 (quotient w 2) (quotient h 2)
	      0 0 w h)
	(send mdc set-bitmap #f)
	(send bm2 set-loaded-mask mbm2))
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
		  (send ed write-to-file stream))
		editors)
      (write-editor-global-footer stream)
      (send base get-bytes)))

  (define (string->editor! str defs)
    (let* ([base (make-object editor-stream-in-bytes-base% str)]
           [stream (make-object editor-stream-in% base)])
      (read-editor-version stream base #t)
      (read-editor-global-header stream)
      (send defs read-from-file stream)
      (read-editor-global-footer stream)))

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

      (define tool-button-label (bitmap-label-maker button-label/h handin-icon))

      (define (make-new-unit-frame% super%)
	(class super%
	  (inherit get-button-panel
		   get-definitions-text
		   get-interactions-text)
	  (super-instantiate ())

          (define/override (file-menu:between-open-and-revert file-menu)
            (new menu-item%
		 (label (format "Manage ~a Handin Account..." handin-name))
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
			       (new handin-frame%
                                    [parent this]
                                    [content content]
                                    [open-drscheme-window
                                     drscheme:unit:open-drscheme-window])))]
		 [style '(deleted)]))

	  (send (get-button-panel) change-children
		(lambda (l) (cons button l)))))

      (when (and server port-no)
        (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f)))))
